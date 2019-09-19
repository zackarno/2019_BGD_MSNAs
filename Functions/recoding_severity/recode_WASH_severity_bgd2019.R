
# hh_data<-HH_with_composite
# individual_data<- indiv_yes_consent
recode_WASH_severity_bgd2019<-name <- function(hh_data,individual_data, population) {
  
  # hh_data$New_Camp_N %>% unique()
  
  # common intermediate variables -------------------------------------------
  
  HH_rec_step1<-hh_data%>% 
    mutate(
    ) 
  
  # Individual intermediate aggregations ------------------------------------
  
  
  # Refugee Only ------------------------------------------------------------

  if(population=="Refugee"){
   hh_data<- hh_data %>% 
      mutate(
        int.wash.sub.living_standards.quantity.s2 = I.WASH.enough_water_all_needs.HH=="yes",
        int.wash.sub.living_standards.quantity.s3 = I.WASH.enough_drinking_and_one_other.HH=="yes"|I.WASH.enough_drinking_and_two_other.HH== "yes",
        int.wash.sub.living_standards.quantity.s4 = I.WASH.enough_drinking_only.HH=="yes",
        int.wash.sub.living_standards.quantity.s5 = enough_water.drinking==0,
        int.wash.sub.living_standards.quantity.s6 = enough_water.none_previous==1 ,
        
        int.wash.sub.living_standards.quantity=if_else(int.wash.sub.living_standards.quantity.s6==1,6,
                                            if_else(int.wash.sub.living_standards.quantity.s5==1,5,
                                                    if_else(int.wash.sub.living_standards.quantity.s4==1,4,
                                                            if_else(int.wash.sub.living_standards.quantity.s3==1, 3,
                                                                    if_else(int.wash.sub.living_standards.quantity.s2==1,2,99))))),
        
        int.wash.sub.living_standards.sanitation.s2 =defecation %in% c("pit_with_slab", "hh_latrine", "dont_know"),
        # int.wash.sub.living_standards.sanitation.s3= defecation =="pit_without_slab",
        int.wash.sub.living_standards.sanitation.s4 = defecation %in% c("pit_without_slab","hanging_toilet","bucket","bag"),
        # int.wash.sub.living_standards.sanitation.s5 = defecation %in%  c("hanging_toilet","bucket"),
        int.wash.sub.living_standards.sanitation.s6 = defecation %in% c("open_defecation", "none"),
        
        int.wash.sub.living_standards.sanitation=if_else(int.wash.sub.living_standards.sanitation.s6==1,6,
                                            # if_else(int.wash.sub.living_standards.sanitation.s5==1,5,
                                                            if_else(int.wash.sub.living_standards.sanitation.s4==1, 4,
                                                                    if_else(int.wash.sub.living_standards.sanitation.s2==1,2,99))),
        
        int.wash.sub.living_standards.soap= if_else(soap=="yes",2,
                                                       if_else(soap=="no", 5,99)),
        
        
        
        vis.faeces= visible_faeces== "yes",
        vis.waste= visible_waste=="yes",
        vis.stagwater= stagnant_water=="yes")
   
   # hh_data %>% filter(int.wash.sub.living_standards.quantity==6) %>% select(camp_name) %>% arrange(camp_name)
   # hh_data %>% group_by(int.wash.sub.living_standards.quantity) %>% count()
   
   
   hh_data<-hh_data %>% 
     mutate(
      
       # int.sum_visible= rowSums(.[c("vis.faeces","vis.waste", "vis.stagwater")], na.rm=TRUE),
       int.wash.sub.living_standards.environment= if_else(vis.faeces,5,
                                                          if_else(vis.waste&vis.stagwater,4,
                                                                  if_else(vis.stagwater|vis.waste,2,1,99))),
       # int.wash.sub.living_standards.environment = if_else(int.sum_visible<2,1,
       #                                                        if_else(int.sum_visible==2,3,
       #                                                                if_else(int.sum_visible==3,4,99))),
       
       int.wash.drnk_pipe_wash_pipe= water_source_drink.pipe_water==1 &  water_source_wash.pipe_water==1,
       
       int.wash.improved_water_drinking= water_source_drink.tubewells | water_source_drink.protected_spring|
         water_source_drink.cart_small_tank| water_source_drink.protected_dugwell|water_source_drink.tanker_truck,
       
       int.wash.improved_water_wash= water_source_wash.tubewells | water_source_wash.protected_spring|
         water_source_wash.cart_small_tank| water_source_wash.protected_dugwell|water_source_wash.tanker_truck,
       
       int.wash.unimproved_water_drinking= water_source_drink.unprotected_dugwell|
         water_source_drink.unprotected_spring,
         # water_source_drink.tanker_truck| 
         # water_source_drink.bottled_water,
       
       int.wash.unimproved_water_wash= water_source_wash.unprotected_dugwell|
         water_source_wash.unprotected_spring,
         # water_source_wash.tanker_truck| 
         # water_source_wash.bottled_water,
       
       int.wash.time_greater_30_min =  if_else(water_source_time=="30_plus_mins",1,0),
       
       int.wash.sub.living_standards.water_source.s2=(int.wash.improved_water_drinking|water_source_drink.pipe_water)&
         (int.wash.time_greater_30_min==0| int.wash.unimproved_water_wash| water_source_wash.surface_water),
         
        
        int.wash.sub.living_standards.water_source.s3= (int.wash.improved_water_drinking|water_source_drink.pipe_water)&
         int.wash.time_greater_30_min==0 & (int.wash.unimproved_water_wash|water_source_wash.surface_water),
         
    
        
        int.wash.sub.living_standards.water_source.s4=(int.wash.improved_water_wash& int.wash.improved_water_drinking ) & 
         int.wash.time_greater_30_min==1, 
        
        int.wash.sub.living_standards.water_source.s5= int.wash.unimproved_water_drinking,
        
        int.wash.sub.living_standards.water_source.s6 = water_source_drink.surface_water, #| water_source_wash.surface_water 
       
       sev_score.wash.water_source=if_else(int.wash.sub.living_standards.water_source.s6==1,6,
                                    if_else(int.wash.sub.living_standards.water_source.s5==1,5,
                                            if_else(int.wash.sub.living_standards.water_source.s4==1,4,
                                                    if_else(int.wash.sub.living_standards.water_source.s3==1, 3,
                                                            if_else(int.wash.sub.living_standards.water_source.s2==1,2,99))))),
       
       int.wash.sub.coping.water = if_else(surface_water_access %in% c("never", "dont_know"),1,
                                           if_else(surface_water_access== "couple_times",3,
                                                   if_else(surface_water_access== "almost_everyday",5,99))),
       int.was.sub.coping.hygiene = if_else(exp_hygiene !=  "0_bdt",3,1),
       #BUILD THIS FUCKING WEIGHT
       index.wash.unsafe_male = if_else(feel_unsafe_male.latrine| feel_unsafe_male.water_points| feel_unsafe_male.bathing_areas| 
                 feel_unsafe_male.distribution_points|feel_unsafe_male.way_to_facilities,2,
                 if_else(feel_unsafe_male.market|feel_unsafe_male.market,1,0)),
       index.wash.unsafe_female=if_else(feel_unsafe_female.latrine| feel_unsafe_female.water_points| feel_unsafe_female.bathing_areas| 
                                          feel_unsafe_female.distribution_points|feel_unsafe_female.way_to_facilities,3,if_else(
                                            feel_unsafe_female.market|feel_unsafe_female.market,1,0)),
       
       index.wash.unsafe_male_reason= if_else(unsafe_reason_male.lack_of_light==1,2,
                                              if_else(unsafe_reason_male.intra_community_tension==1,1,0)),
       
       index.wash.unsafe_female_reason= if_else(unsafe_reason_female.lack_of_light==1,3,
                                              if_else(unsafe_reason_female.intra_community_tension==1,1,0)),
       index.wash.women_market_access= if_else(unmarried_market_mobility=="never_alone" | 
                                               married_market_mobility=="never_alone",4,
                                               if_else(unmarried_market_mobility!= "not_alone"|
                                                         married_market_mobility!= "not_alone",2,0)),
       index.wash.no_portable_lights=if_else(portable_light== 0, 3,0),
       index.wash.barriers_to_humanitarian_workers= if_else (aidworkers_barriers.none==0,2,0),
       
       index.wash.going_well= if_else( aid_goingwell.clean_water==1, -3,
                                       if_else(aid_goingwell.improved_sanitation==1,-2,
                                               if_else(aid_goingwell.ngo_training==1|aid_goingwell.employment_access==1|
                                                         aid_goingwell.more_safespaces_c==1|aid_goingwell.aid_organised==1,-1,0))),
       
       index.wash.not_going_well=if_else(aid_notgoingwell.clean_water==1,4,
                                         if_else(aid_notgoingwell.poor_sanitation==1,3,
                                                 if_else(aid_notgoingwell.no_employment_access==1,2,
                                                         if_else(aid_notgoingwell.no_prep_natural_disaster==1|aid_notgoingwell.safety_concerns==1|
                                                           aid_notgoingwell.aid_disorganised==1|
                                                           aid_notgoingwell.bad_relationships==1|
                                                           aid_notgoingwell.damaged_inappropriate==1|
                                                           aid_notgoingwell.no_info==1,1,0))))
       )
   hh_data %>% group_by(sev_score.wash.water_source) %>% count()
  wash_indices<- c("index.wash.unsafe_male", "index.wash.unsafe_female", "index.wash.unsafe_male_reason", 
     "index.wash.unsafe_female_reason", "index.wash.women_market_access", 
     "index.wash.no_portable_lights", "index.wash.barriers_to_humanitarian_workers", 
     "index.wash.going_well", "index.wash.not_going_well")
  
  hh_data<-hh_data %>% 
    mutate(
      indices_summed.wash= rowSums(.[wash_indices],na.rm=TRUE),
      int.wash.sub.wellbeing.safety_index= if_else(indices_summed.wash<1,1,
                                         if_else(indices_summed.wash <7,2,
                                                 if_else(indices_summed.wash<14,3,
                                                         if_else(indices_summed.wash<22,4,
                                                                 if_else(indices_summed.wash<36,5,99))))),
      
      under5_diar=ifelse(is.na(under5_diarrhea),0,under5_diarrhea),

      int.wash.sub.wellbeing.diarh= if_else(under5_diar>0  & over5_diarrhea>0,5,
                                            if_else(under5_diar>0 & over5_diarrhea==0, 4,
                                                    if_else(under5_diar==0 & over5_diarrhea>0,3,1)))
    ) 





    
  }
  
 
  
  
  if(population=="Host"){
    HH_rec_step1$shelter_typology<-calculate_shelter_typology(HH_rec_step1)
    HH_rec_step2<-HH_rec_step1 %>% 
      mutate(
        #STRUCTURE

  }
  HH_rec_step3
  
}
