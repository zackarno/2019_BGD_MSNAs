# 
# hh_data<-ref
# individual_data<-ref_indiv
# population=="Refugee"
# hh_data
# 
# # hh_data<-host
# # individual_data<-host_indiv
# # population=="Host"

recode_EDUCATION_severity_bgd2019<-name <- function(hh_data,individual_data, population) {
  
  if(population=="Refugee"){
    core_age<-5:17
    outside_core_age<- c(0:4, 18:100)
  }
  if( population=="Host"){
    core_age<-5:17
    outside_core_age<- c(0:4, 18:100)
  }
  
  # common intermediate variables -------------------------------------------

  

 
  
  # Refugee Only ------------------------------------------------------------
  
  
  
  if(population=="Refugee"){
    indiv<-individual_data
    
    school_options<- c("int.ed_TLC.yes","ind_ed_nonformal.yes","ind_ed_madrassa.yes")
    indiv_to_indiv<-indiv %>%
      mutate(
        int.ed_TLC.yes= ind_ed_TLC=="yes",
        ind_ed_nonformal.yes=ind_ed_nonformal== "yes",
        ind_ed_madrassa.yes=ind_ed_madrassa== "yes",
        ind_core_age=ind_age %in% core_age)
     
    indiv_to_indiv<-indiv_to_indiv %>% 
      
      mutate(
        
        # int.education.indiv.no_education = ifelse(rowSums(.[school_options],na.rm = TRUE)==0,1,0),
        int.education.indiv.no_education = ind_ed_TLC== "no" &  ind_ed_nonformal== "no" & ind_ed_madrassa=="no",
        int.education.indiv_under18_no_school= if_else( ind_age<18 & int.education.indiv.no_education,1,0,missing=0),
        int.education.indiv.non_formal_education_only= ( ind_ed_madrassa== "yes" | ind_ed_nonformal== "yes") & ind_ed_TLC== "no",
        
        int.education.indiv.core_age_sev4= ind_core_age &int.education.indiv.no_education,
        
        
        int.education.indiv.sev3=if_else((ind_core_age==FALSE &int.education.indiv.no_education)|
          (int.education.indiv.non_formal_education_only & ind_core_age),1,0,missing=0),
        # int.education.indiv.outside_core_age_sev3= ind_core_age==FALSE &int.education.indiv.no_education,
        # int.education.indiv.core_age_sev3=int.education.indiv.non_formal_education_only & ind_core_age,
        
        # int.education.indiv.core_age_sev2=ind_core_age & int.ed_TLC.yes , 
        # int.education.indiv.outside_core_age_sev2=ind_core_age== FALSE & int.education.indiv.no_education==0
        int.education.indiv.sev2= if_else((ind_core_age & int.ed_TLC.yes)|(ind_core_age== FALSE & int.education.indiv.no_education==0),1,0,missing=0),
        # int.education.not_applicable= is.na(ind_ed_TLC),
        int.education.indiv.sev1= is.na(ind_ed_TLC),
        int.education.indiv.sev_score= if_else(int.education.indiv.core_age_sev4==1,4,
                                               if_else(int.education.indiv.sev3==1,3, 
                                                       if_else(int.education.indiv.sev2==1,2,
                                                               if_else(int.education.indiv.sev1==1,1,99))))
        
      )
    indiv_to_indiv %>% group_by(int.education.indiv.core_age_sev4) %>% count()
    indiv_to_indiv %>% group_by(int.education.indiv.sev3) %>% count()
    indiv_to_indiv %>% group_by(int.education.indiv.sev2) %>% count()
    indiv_to_indiv %>% group_by(int.education.indiv.sev1) %>% count()
    indiv_to_indiv %>% group_by(int.education.indiv.sev_score) %>% count()
    
    # indiv_to_indiv %>% select(starts_with("int.education")) %>% filter(int.education.indiv.sev_score==3) %>% head()
    
    indiv_to_hh<-indiv_to_indiv %>% 
      group_by(X_submission__uuid) %>% 
      summarise(
        sev_score.education.sub.attendance=max(int.education.indiv.sev_score,na.rm = TRUE),
        int.education.atleast_one_under18_out_of_school= if_else(sum(int.education.indiv_under18_no_school,na.rm=0)>0,1,0)
      )
    
    
    
    HH_data_with_aggregated_individual<-left_join(hh_data, indiv_to_hh , by= c("X_uuid"="X_submission__uuid"))
    HH_rec_step1<-HH_data_with_aggregated_individual%>% 
      mutate(
        sev_score.education.sub.barriers=if_else(education_barrier.gets_married &
                                                   int.education.atleast_one_under18_out_of_school,4,1, missing=0),
        sev_score.education.total= if_else(sev_score.education.sub.barriers>sev_score.education.sub.attendance, 
                                           sev_score.education.sub.barriers,sev_score.education.sub.attendance)
    
      )
    
    
    HH_rec_step2<-HH_rec_step1 %>% 
      mutate(

        
      ) 
    
    HH_rec_step3<-HH_rec_step2 %>% 
      mutate(

      ) 
    
  }
 

# Host only ---------------------------------------------------------------

  
  if(population=="Host"){
    indiv<-individual_data
    
    # school_options<- c("int.ed_formal.yes","ind_ed_nonformal.yes","ind_ed_madrassa.yes")
    
    indiv_to_indiv<-indiv %>%
      mutate(
        
        int.ed_formal.yes=ind_formal_learning!="none" & !is.na(ind_formal_learning),
        int.ed_formal.no= ind_formal_learning=="none",
        ind_ed_nonformal.yes=ind_nonformal_learn.none==0 ,
        ind_ed_nonformal.no=ind_nonformal_learn.none==1|is.na(ind_nonformal_learn.none),
        ind_core_age=ind_age %in% core_age,
        
        int.education.indiv_under18_no_school= if_else( ind_age<18 & int.ed_formal.no  & ind_ed_nonformal.no,1,0,missing=0),
        
        int.education.indiv.sev4 = if_else(ind_core_age & int.ed_formal.no & ind_ed_nonformal.no,1,0,missing=0),
        
        int.education.indiv.sev3 = if_else((ind_core_age & int.ed_formal.no & ind_ed_nonformal.yes)|
          (ind_core_age==FALSE & int.ed_formal.no & ind_ed_nonformal.no),1,0,missing=0),
        
        int.education.indiv.sev2 = if_else(ind_core_age==FALSE & int.ed_formal.no==TRUE & ind_ed_nonformal.yes==TRUE,1,0,missing = 0),
    
        int.education.not_applicable= is.na(ind_formal_learning),
        int.education.indiv.sev1=int.ed_formal.yes|int.education.not_applicable,
   
        int.education.indiv.sev_score= if_else(int.education.indiv.sev4==1,4,
                                               if_else(int.education.indiv.sev3==1,3,
                                                       if_else(int.education.indiv.sev2==1,2,
                                                               if_else(int.education.indiv.sev1==1,1,99,missing=1000))))
        
      )
    indiv_to_indiv %>% group_by(int.education.indiv.sev_score) %>% count()
  

    
    indiv_to_hh<-indiv_to_indiv %>% 
      group_by(X_submission__uuid) %>% 
      summarise(
        sev_score.education.sub.attendance=max(int.education.indiv.sev_score,na.rm = TRUE),
        int.education.atleast_one_under18_out_of_school= if_else(sum(int.education.indiv_under18_no_school,na.rm=0)>0,1,0)
      )
    HH_data_with_aggregated_individual<-left_join(hh_data, indiv_to_hh , by= c("X_uuid"="X_submission__uuid"))
    HH_rec_step1<-HH_data_with_aggregated_individual%>% 
      mutate(
      sev_score.education.sub.barriers=if_else((education_barrier.gets_married|education_barrier.child_income) &
                                                 int.education.atleast_one_under18_out_of_school,4,1, missing=0),
      sev_score.education.total= if_else(sev_score.education.sub.barriers>sev_score.education.sub.attendance, 
                                         sev_score.education.sub.barriers,sev_score.education.sub.attendance),
      
      sev_score.education.total_over_3= if_else(sev_score.education.total>3,1,0)
    
      
        
      ) 
    
    
    
    
    HH_rec_step2<-HH_rec_step1 %>% 
      mutate(

        
      ) 

    HH_rec_step3<-HH_rec_step2 %>% 
      mutate(

      ) 
    
  }
  HH_rec_step3
  
}
