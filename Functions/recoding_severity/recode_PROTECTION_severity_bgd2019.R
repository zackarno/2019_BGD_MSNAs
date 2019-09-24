# host<-HH_with_composite# # ref<-HH_with_composite
# # ref_indiv<- indiv_yes_consent
# # ref %>% select(starts_with("feel_unsafe_female.")) %>% colnames() %>% dput()
# 
# hh_data<-HH_with_composite
# individual_data<- indiv_yes_consent

# host_indiv<- indiv_yes_consent

# hh_data %>% filter(is.na(HH_distress.violence)) %>%  select(survey_date,camp_name)

# ms()
recode_PROTECTION_severity_bgd2019<- function(hh_data,individual_data, population,incident_data) {

  protection_incidences_per_camp<-incidents_per_camp
  
    
  
  hazardous_work<-c("ind_work_situation.machine_heavy_objects", "ind_work_situation.chemicals", 
                            "ind_work_situation.heat", "ind_work_situation.more_40_hours", 
                            "ind_work_situation.work_night", "ind_work_situation.sharp_objects", 
                            "ind_work_situation.none", "ind_work_situation.dntknow_prefer")
  
  if(population=="Host"){
    hazardous_work_prefix<-"ind_work_situation."
    hh_data<-hh_data %>% 
      mutate(
        int.protection.sub.coping.child_firewood =if_else(wood_access %in% c("boy", "girl"), 1, 0, missing=0)
      )
  }
  if (population=="Refugee"){
    hazardous_work_prefix<- "ind_work_hazardous."
    
    hh_data<-hh_data %>% 
      left_join(protection_incidences_per_camp %>% 
                  select(camp,incidents_per_1000_hh), by= c("camp_name"= "camp")) 
    
    
    hh_data<-hh_data %>% 
      mutate(
        int.protection.sub.coping.child_firewood=if_else(wood_access.boys|wood_access.girls,1,0, missing = 0),
        int.sev_score.protection.sub.security.number_incidents= if_else(incidents_per_1000_hh>=4,6,
                                                                        if_else(incidents_per_1000_hh>3,5,
                                                                                if_else(incidents_per_1000_hh>2,4,
                                                                                        if_else(incidents_per_1000_hh>1,3,
                                                                                                if_else(incidents_per_1000_hh>0,2,
                                                                                                        if_else(incidents_per_1000_hh==0,1,99)))))),
        int.sev_pts.protection.sub.security.number_incidents= if_else(incidents_per_1000_hh>=4,15,
                                                                      if_else(incidents_per_1000_hh>3,12,
                                                                              if_else(incidents_per_1000_hh>2,9,
                                                                                      if_else(incidents_per_1000_hh>1,6,
                                                                                              if_else(incidents_per_1000_hh>0,3,
                                                                                                      if_else(incidents_per_1000_hh==0,0,99)))))),
        
      )
    
    
    
  }
  
  #CLASSIFY DIFFERENT TYPES OF HAZARDOUS WORK
  hazardous_work_cols_s1<-paste0(hazardous_work_prefix,c("none", "dntknow_prefer"))
  hazardous_work_cols_s2<-paste0(hazardous_work_prefix,c("sharp_objects","machine_heavy_objects"))
  hazardous_work_cols_s3<-paste0(hazardous_work_prefix,c("more_40_hours"))
  hazardous_work_cols_s4<-paste0(hazardous_work_prefix,"work_night")
  hazardous_work_cols_s5<-paste0(hazardous_work_prefix,c("heat","chemicals"))
  # population
                                  
  
  
  # bquote(hazardous_work_types_s1_expression)
  
  
  
  unsafe_locations_male_female<-c("feel_unsafe_female.shelter", "feel_unsafe_female.latrine", 
                                  "feel_unsafe_female.market", "feel_unsafe_female.health_center", 
                                  "feel_unsafe_female.water_points", "feel_unsafe_female.bathing_areas", 
                                  "feel_unsafe_female.spaces", "feel_unsafe_female.firewood_site", 
                                  "feel_unsafe_female.inside_home", "feel_unsafe_female.way_to_facilities", 
                                  "feel_unsafe_female.other","feel_unsafe_male.shelter", "feel_unsafe_male.latrine", "feel_unsafe_male.market", 
                                  "feel_unsafe_female.distribution_points",
                                  "feel_unsafe_male.health_center", "feel_unsafe_male.water_points", 
                                  "feel_unsafe_male.bathing_areas", "feel_unsafe_male.spaces", 
                                  "feel_unsafe_male.firewood_site", "feel_unsafe_male.inside_home", 
                                  "feel_unsafe_male.way_to_facilities","feel_unsafe_male.other","feel_unsafe_male.distribution_points")
  tension_columns<-c("rohi_hc_tensions.falling_wage", "rohi_hc_tensions.security_concerns", 
                     "rohi_hc_tensions.health_cost", "rohi_hc_tensions.transport_cost", 
                     "rohi_hc_tensions.difficult_up_services", "rohi_hc_tensions.decline_moral_standards", 
                     "rohi_hc_tensions.environmental_degradation", "rohi_hc_tensions.price_hike", 
                     "rohi_hc_tensions.less_grazing_land", "rohi_hc_tensions.family_dispute", 
                     "rohi_hc_tensions.crime_increase", "rohi_hc_tensions.impact_education", 
                     "rohi_hc_tensions.other")
  
  gbv_support_columns<-c("gbv_support.health_facilities", "gbv_support.psychosocial_support", 
                         "gbv_support.police", "gbv_support.legal_aid", "gbv_support.dispute_resolution", 
                         "gbv_support.other", "gbv_support.majhee")
  
  gbv_key_support_columns<-c("gbv_support.health_facilities", "gbv_support.psychosocial_support", 
                             "gbv_support.police", "gbv_support.legal_aid")
  gbv_support_columns<-names(hh_data)[names(hh_data)%in%gbv_support_columns %>%which()]
  gbv_non_key_support<-setdiff(gbv_support_columns,gbv_key_support_columns)
  
  unsafe_locations_male_female<-names(hh_data)[names(hh_data)%in%unsafe_locations_male_female %>%which()]
  
  
  hh_data<-hh_data %>% 
    mutate(
      int.protection.sub.security.number_unsafe_rowsum= rowSums(.[unsafe_locations_male_female], na.rm=TRUE),
      int.sev_score.protection.sub.security.number_unsafe= if_else(int.protection.sub.security.number_unsafe_rowsum==0,1,
                                                                   if_else(int.protection.sub.security.number_unsafe_rowsum==1,2,
                                                                           if_else(int.protection.sub.security.number_unsafe_rowsum<4,3,
                                                                                   if_else(int.protection.sub.security.number_unsafe_rowsum<7,4,
                                                                                           if_else(int.protection.sub.security.number_unsafe_rowsum<10,5,
                                                                                                   if_else(int.protection.sub.security.number_unsafe_rowsum>=10,6,99)))))),
      int.sev_pts.protection.sub.security.number_unsafe= if_else(int.protection.sub.security.number_unsafe_rowsum==0,0,
                                                                 if_else(int.protection.sub.security.number_unsafe_rowsum==1,2,
                                                                         if_else(int.protection.sub.security.number_unsafe_rowsum<4,4,
                                                                                 if_else(int.protection.sub.security.number_unsafe_rowsum<7,6,
                                                                                         if_else(int.protection.sub.security.number_unsafe_rowsum<10,8,
                                                                                                 if_else(int.protection.sub.security.number_unsafe_rowsum>=10,10,99)))))),
      int.protection.sub.services.gbv.key_support_rowsums = rowSums(.[gbv_key_support_columns],na.rm=TRUE) ,
      int.protection.sub.services.gbv.non_key_support_rowsums = rowSums(.[gbv_non_key_support],na.rm=TRUE),
      
      int.sev_score.protection.sub.services.gbv= if_else(int.protection.sub.services.gbv.key_support_rowsums==4,1,
                                                         if_else(int.protection.sub.services.gbv.key_support_rowsums==3,2,
                                                                 if_else(int.protection.sub.services.gbv.key_support_rowsums==2,3,
                                                                         if_else(int.protection.sub.services.gbv.key_support_rowsums==1,4,
                                                                                 if_else(int.protection.sub.services.gbv.non_key_support_rowsums>0,5,
                                                                                         if_else(gbv_support.dont_know==1,6,99)))))),
      int.sev_pts.protection.sub.services.gbv=if_else(int.protection.sub.services.gbv.key_support_rowsums==4,0,
                                                      if_else(int.protection.sub.services.gbv.key_support_rowsums==3,3,
                                                              if_else(int.protection.sub.services.gbv.key_support_rowsums==2,6,
                                                                      if_else(int.protection.sub.services.gbv.key_support_rowsums==1,9,
                                                                              if_else(int.protection.sub.services.gbv.non_key_support_rowsums>0, 12,5,
                                                                                      if_else(gbv_support.dont_know==1,15,99)))))),
      
      int.protection.sub.mental_phys.child_marriage= if_else(child_marriage=="yes",1,0, missing=0),
      int.protection.sub.mental_phys.separated_child= I.HH.CP.sep_unaccom_minor_atleast_one.INDVHH=="yes"
    )
  

  
  disab_needs<-c("ind_help_daily_reason.person_elderly", "ind_help_daily_reason.person_long_pain", 
                 "ind_help_daily_reason.physical_disability", "ind_help_daily_reason.mental_health", 
                 "ind_help_daily_reason.other")

 
 
 
 individual_data<-individual_data %>% 
   mutate(
     hazardous_work_types_s1 = eval(parse(text=paste(hazardous_work_cols_s1, collapse="|"))),
     hazardous_work_types_s2 = eval(parse(text=paste(hazardous_work_cols_s2, collapse="|"))),
     hazardous_work_types_s3 = eval(parse(text=paste(hazardous_work_cols_s3, collapse="|"))),
     hazardous_work_types_s4 = eval(parse(text=paste(hazardous_work_cols_s4, collapse="|"))),
     hazardous_work_types_s5 = eval(parse(text=paste(hazardous_work_cols_s5, collapse="|")))
    )
 
 ind_to_hh<-individual_data %>% 
   group_by(X_submission__uuid) %>% 
    summarise_at(
      vars(hazardous_work_types_s1,hazardous_work_types_s2,hazardous_work_types_s3,hazardous_work_types_s4,hazardous_work_types_s5), function(x) if_else(any(x), 1,0,missing=0)
      
    )
   
 
 hh_data<-left_join(hh_data, ind_to_hh, by=c("X_uuid"="X_submission__uuid"))

 
 hh_data<-hh_data %>% 
  mutate(

    int.protection.sub.coping.hazardous_work = if_else(hazardous_work_types_s5==1,5,
                                                  if_else(hazardous_work_types_s4==1,4,
                                                          if_else(hazardous_work_types_s3==1,3,
                                                                  if_else(hazardous_work_types_s2==1,2,
                                                                          if_else(hazardous_work_types_s1==1,1,1))))),
    
    int.sev_score.protection.sub.coping = if_else(int.protection.sub.coping.hazardous_work==5,5,
                                                  if_else(int.protection.sub.coping.hazardous_work==4,4,
                                                          if_else(int.protection.sub.coping.hazardous_work|int.protection.sub.coping.child_firewood==3,3,
                                                                  if_else(int.protection.sub.coping.hazardous_work==2,2,
                                                                          if_else (int.protection.sub.coping.child_firewood==FALSE &  int.protection.sub.coping.hazardous_work==1,1,99))))),
    
    int.sev_pts.protection.sub.coping = if_else(int.protection.sub.coping.hazardous_work==5,23,
                                                  if_else(int.protection.sub.coping.hazardous_work==4,19,
                                                          if_else(int.protection.sub.coping.child_firewood|int.protection.sub.coping.hazardous_work==3,13,
                                                                  if_else(int.protection.sub.coping.hazardous_work==2,7,
                                                                          if_else (int.protection.sub.coping.child_firewood==FALSE &  int.protection.sub.coping.hazardous_work==1,0,99))))),
                                                                                  

    
  )

ind_to_hh <-individual_data %>% 
  group_by(X_submission__uuid) %>% 
    summarise_at(
      disab_needs, function(x) if_else(any(x),1,0, missing=0)
    ) %>% 
    mutate(
      int.protection.sub.services.pwd.number_assistance_types= rowSums(.[disab_needs], na.rm=TRUE)
    ) 

hh_data<-left_join(hh_data ,ind_to_hh,  by=c("X_uuid"="X_submission__uuid")) %>% 
    mutate(
      # int.sev_score.protection.sub.security.number_unsafe
      int.sev_score.protection.sub.services.pwd= if_else(int.protection.sub.services.pwd.number_assistance_types==0,1,
                                                         if_else(int.protection.sub.services.pwd.number_assistance_types==1 & disability_support== "yes",2,
                                                                 if_else(int.protection.sub.services.pwd.number_assistance_types>1 & disability_support== "yes",3,
                                                                         if_else(int.protection.sub.services.pwd.number_assistance_types<3 & disability_support== "no",4,
                                                                                 if_else(int.protection.sub.services.pwd.number_assistance_types<5 & disability_support== "no",5,
                                                                                         if_else(int.protection.sub.services.pwd.number_assistance_types==5 & disability_support== "no",6,99)))))),
      int.sev_pts.protection.sub.services.pwd= if_else(int.protection.sub.services.pwd.number_assistance_types==0,0,
                                                       if_else(int.protection.sub.services.pwd.number_assistance_types==1 & disability_support== "yes",2,
                                                               if_else(int.protection.sub.services.pwd.number_assistance_types>1 & disability_support== "yes",4,
                                                                       if_else(int.protection.sub.services.pwd.number_assistance_types<3 & disability_support== "no",6,
                                                                               if_else(int.protection.sub.services.pwd.number_assistance_types<5 & disability_support== "no",8,
                                                                                       if_else(int.protection.sub.services.pwd.number_assistance_types==5 & disability_support== "no",10,99)))))),
      
      
    )

  
  if(population=="Host"){
    hh_data<-hh_data %>% 
      mutate(
        int.protection.sub.security.tensions_rowsum= rowSums(.[tension_columns], na.rm=TRUE),
        int.sev_score.protection.sub.security.tensions=if_else(is.na(int.protection.sub.security.tensions_rowsum),1,
                                                               if_else(int.protection.sub.security.tensions_rowsum==1,2,
                                                                       if_else(int.protection.sub.security.tensions_rowsum<4,3,
                                                                               if_else(int.protection.sub.security.tensions_rowsum<7,4,
                                                                                       if_else(int.protection.sub.security.tensions_rowsum<10,5,
                                                                                               if_else(int.protection.sub.security.tensions_rowsum>=10,6,99)))))),
        int.sev_pts.protection.sub.security.tensions=if_else(is.na(int.protection.sub.security.tensions_rowsum),0,
                                                             if_else(int.protection.sub.security.tensions_rowsum==1,3,
                                                                     if_else(int.protection.sub.security.tensions_rowsum<4,6,
                                                                             if_else(int.protection.sub.security.tensions_rowsum<7,9,
                                                                                     if_else(int.protection.sub.security.tensions_rowsum<10,12,
                                                                                             if_else(int.protection.sub.security.tensions_rowsum>=10,15,99)))))),
        
        

        
        int.sev_score.protection.mental_phys.s1= int.protection.sub.mental_phys.child_marriage==0 &int.protection.sub.mental_phys.separated_child==0 ,
        
        int.sev_score.protection.mental_phys.s2= int.protection.sub.mental_phys.separated_child,
        
        int.sev_score.protection.mental_phys.s3= int.protection.sub.mental_phys.child_marriage==1,

        
        int.sev_score.protection.mental_phys.s5 = int.protection.sub.mental_phys.separated_child & int.protection.sub.mental_phys.child_marriage,
        
        int.sev_score.protection.mental_phys.total= if_else(int.sev_score.protection.mental_phys.s5==1,5,
                                                                if_else(int.sev_score.protection.mental_phys.s3==1,3,
                                                                        if_else(int.sev_score.protection.mental_phys.s2== 1,2,
                                                                                if_else(int.sev_score.protection.mental_phys.s1==1,1, 99)))),
        int.sev_pts.protection.mental_phys.total= if_else(int.sev_score.protection.mental_phys.s5==1,23,
                                                        if_else(int.sev_score.protection.mental_phys.s3==1,13,
                                                                if_else(int.sev_score.protection.mental_phys.s2== 1,7,
                                                                        if_else(int.sev_score.protection.mental_phys.s1==1,0, 99))))
        
      )
]
  }
  
  if(population=="Refugee"){
    hh_data<-hh_data %>% 
      mutate(
        int.protection.sub.mental_phys.missing_child_logical= if_else(missing_child=="yes", TRUE, FALSE),
        int.protection.sub.mental_phys.hh_distress = if_else(HH_distress.violence==1,1,0, missing=0),
        
        int.sev_score.protection.mental_phys.s1= int.protection.sub.mental_phys.child_marriage==0 &
          int.protection.sub.mental_phys.hh_distress==0 & int.protection.sub.mental_phys.separated_child==0 & int.protection.sub.mental_phys.missing_child_logical==0,
        
        int.sev_score.protection.mental_phys.s2= int.protection.sub.mental_phys.separated_child,
        
        int.sev_score.protection.mental_phys.s3= int.protection.sub.mental_phys.child_marriage==1,
        
        int.sev_score.protection.mental_phys.s4 = int.protection.sub.mental_phys.hh_distress| int.protection.sub.mental_phys.missing_child_logical,
        
        int.sev_score.protection.mental_phys.s5 = (int.protection.sub.mental_phys.separated_child| 
                                                     int.protection.sub.mental_phys.hh_distress|
                                                     int.protection.sub.mental_phys.missing_child_logical)&int.protection.sub.mental_phys.child_marriage,
        
        int.sev_score.protection.mental_phys.total= if_else(int.sev_score.protection.mental_phys.s5==1,5,
                                                           if_else(int.sev_score.protection.mental_phys.s4==1,4,
                                                                   if_else(int.sev_score.protection.mental_phys.s3==1,3,
                                                                           if_else(int.sev_score.protection.mental_phys.s2== 1,2,
                                                                                   if_else(int.sev_score.protection.mental_phys.s1==1,1, 99))))),
        int.sev_pts.protection.mental_phys.total= if_else(int.sev_score.protection.mental_phys.s5==1,23,
                                                            if_else(int.sev_score.protection.mental_phys.s4==1,19,
                                                                    if_else(int.sev_score.protection.mental_phys.s3==1,13,
                                                                            if_else(int.sev_score.protection.mental_phys.s2== 1,7,
                                                                                    if_else(int.sev_score.protection.mental_phys.s1==1,0, 99)))))
                                                                                           
        
      )
    
  }


sev_pt_cols<-hh_data %>% select(contains("sev_pts")) %>% colnames()  

hh_data<-hh_data %>% 
  mutate(sev_score.protection.sum=rowSums(.[sev_pt_cols],na.rm=TRUE),
         sev_score.protection.total=if_else(sev_score.protection.sum>80,6,
                                            if_else(sev_score.protection.sum>50,5,
                                                    if_else(sev_score.protection.sum>35,4,
                                                            if_else(sev_score.protection.sum>20,3,
                                                                    if_else(sev_score.protection.sum>10,2,1))))),
         sev_score.protection.total_greater2= if_else(sev_score.protection.total>2,1,0),
         sev_score.protection.total_greater3= if_else(sev_score.protection.total>=4,1,0)
  )
  

}

