
# hh_data<-HH_with_composite
# individual_data<- indiv_yes_consent
recode_WASH_severity_bgd2019<- function(hh_data,individual_data, population, wash_combo_table) {
  
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

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
                                                    if_else(soap=="no", 4,99)),
        
        
        
        vis.faeces= visible_faeces== "yes",
        vis.waste= visible_waste=="yes",
        vis.stagwater= stagnant_water=="yes")
    
   
     mode_int<-hh_data %>% 
      group_by(!!sym(strata)) %>% 
      summarise(mode.int.wash.sub.living_standards.quantity=getmode(int.wash.sub.living_standards.quantity))
    
    fixed_water_quantity<-hh_data[rows_to_fix,c("X_uuid","camp_name")] %>% left_join(mode_int, by="camp_name")
    
    hh_data$int.wash.sub.living_standards.quantity<-ifelse(hh_data$X_uuid %in%
                                                           fixed_water_quantity$X_uuid,
                                                           mode_int$mode.int.wash.sub.living_standards.quantity,
                                                           hh_data$int.wash.sub.living_standards.quantity) %>% as.numeric()
   
    
   living_standards_subs<-c("int.wash.sub.living_standards.water_source", "int.wash.sub.living_standards.soap", "int.wash.sub.living_standards.quantity","int.wash.sub.living_standards.sanitation", "int.wash.sub.living_standards.environment")
    
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
          water_source_drink.cart_small_tank| water_source_drink.protected_dugwell|water_source_drink.tanker_truck|water_source_drink.pipe_water|water_source_drink.rainwater_collected,
        
        int.wash.improved_water_wash= water_source_wash.tubewells | water_source_wash.protected_spring|
          water_source_wash.cart_small_tank| water_source_wash.protected_dugwell|water_source_wash.tanker_truck|water_source_wash.pipe_water,
        
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
        
        
        
        int.wash.sub.living_standards.water_source.s4=(int.wash.improved_water_wash& (int.wash.improved_water_drinking|water_source_drink.pipe_water) ) & 
          int.wash.time_greater_30_min==1, 
        
        int.wash.sub.living_standards.water_source.s5= int.wash.unimproved_water_drinking,
        
        int.wash.sub.living_standards.water_source.s6 = water_source_drink.surface_water, #| water_source_wash.surface_water 
        
        int.wash.sub.living_standards.water_source=if_else(int.wash.sub.living_standards.water_source.s6==1,6,
                                                           if_else(int.wash.sub.living_standards.water_source.s5==1,5,
                                                                   if_else(int.wash.sub.living_standards.water_source.s4==1,4,
                                                                           if_else(int.wash.sub.living_standards.water_source.s3==1, 3,
                                                                                   if_else(int.wash.sub.living_standards.water_source.s2==1,2,99))))),
        
        sev_score.wash.sub.coping.water = if_else(surface_water_access %in% c("never", "dont_know"),1,
                                                  if_else(surface_water_access== "couple_times",3,
                                                          if_else(surface_water_access== "almost_everyday",5,99))),
        sev_score.was.sub.coping.hygiene = if_else(exp_hygiene !=  "0_bdt",3,1),
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

    wash_indices<- c("index.wash.unsafe_male", "index.wash.unsafe_female", "index.wash.unsafe_male_reason", 
                     "index.wash.unsafe_female_reason", "index.wash.women_market_access", 
                     "index.wash.no_portable_lights", "index.wash.barriers_to_humanitarian_workers", 
                     "index.wash.going_well", "index.wash.not_going_well")
    
    wash_combo_table<-wash_combo_table %>% 
      mutate(coping_wellbeing_combo=paste0(wash_coping, "_", wash_wellbeing))
    
    hh_data<-hh_data %>% 
      mutate(
        indices_summed.wash= rowSums(.[wash_indices],na.rm=TRUE),
        int.wash.sub.wellbeing.safety_index= if_else(indices_summed.wash<1,1,
                                                     if_else(indices_summed.wash <7,2,
                                                             if_else(indices_summed.wash<13,3,
                                                                     if_else(indices_summed.wash<20,4,
                                                                             if_else(indices_summed.wash<36,5,99))))),
        
        under5_diar=ifelse(is.na(under5_diarrhea),0,under5_diarrhea),
        
        int.wash.sub.wellbeing.diarh= if_else(under5_diar>0  & over5_diarrhea>0,5,
                                              if_else(under5_diar>0 & over5_diarrhea==0, 4,
                                                      if_else(under5_diar==0 & over5_diarrhea>0,3,1))),
        
        #LIVING STANDARDS CUMULATIVE
        
        living_standards_subs_rowsums= rowSums(.[living_standards_subs], na.rm=TRUE),
        living_standards_subs_rowsums5=rowSums(sapply(hh_data[,living_standards_subs], function(x) ifelse(x==5, TRUE,FALSE))),
        living_standards_subs_rowsums4=rowSums(sapply(hh_data[,living_standards_subs], function(x) ifelse(x==4, TRUE,FALSE))),
        living_standards_subs_rowsums3=rowSums(sapply(hh_data[,living_standards_subs], function(x) ifelse(x==3, TRUE,FALSE))),
        living_standards_subs_rowsums2=rowSums(sapply(hh_data[,living_standards_subs], function(x) ifelse(x==2, TRUE,FALSE))),
        living_standards.s6<-if_else((int.wash.sub.living_standards.quantity==6 |
                                        int.wash.sub.living_standards.water_source==6 |int.wash.sub.living_standards.sanitation==6)|
                                       living_standards_subs_rowsums5>1,1,0),
        six_test=if_else(int.wash.sub.living_standards.quantity==6 |
                           int.wash.sub.living_standards.water_source==6 |int.wash.sub.living_standards.sanitation==6,1,0),
        five_test=if_else(living_standards_subs_rowsums5>1,1,0), 
        living_standards.s5<- if_else(living_standards_subs_rowsums5>0,1,0),
        living_standards.s4<- if_else(living_standards_subs_rowsums4>1,1,0),
        living_standards.s3<- if_else(living_standards_subs_rowsums3>1|living_standards_subs_rowsums4==1, 1,0),
        living_standards.s2<- if_else(living_standards_subs_rowsums2>1|living_standards_subs_rowsums3==1, 1,1, missing = 99),
        sev_score.living_standards.subtotal=if_else(living_standards.s6==1,6,
                                                    if_else(living_standards.s5==1,5,
                                                            if_else(living_standards.s4==1, 4,
                                                                    if_else(living_standards.s3==1, 3,
                                                                            if_else(living_standards.s2==1,2,99))))))
    hh_data<-hh_data %>% 
      mutate(
        sev_score.coping.subtotal=  apply(hh_data[,c("sev_score.wash.sub.coping.water","sev_score.was.sub.coping.hygiene")],1,max) ,
        sev_score.wellbeing.subtotal=  apply(hh_data[,c("int.wash.sub.wellbeing.safety_index","int.wash.sub.wellbeing.diarh")],1,max),
        coping_wellbeing_combo= paste0(sev_score.coping.subtotal,"_",sev_score.wellbeing.subtotal),
        
      )
    
    hh_data<-hh_data %>% 
      left_join(
        wash_combo_table %>% 
          select(coping_wellbeing_combo, Final), by= "coping_wellbeing_combo")
    
    hh_data<-hh_data %>% 
      mutate(
        sev_score.wash.total=ifelse(sev_score.living_standards.subtotal > sev_score.coping.subtotal|
                                      sev_score.living_standards.subtotal > sev_score.wellbeing.subtotal,sev_score.living_standards.subtotal,Final)
      )
  }
  

  if(population=="Host"){
    hh_data<- hh_data %>% 
      mutate(
        int.wash.sub.living_standards.quantity.s1 = I.WASH.enough_water_all_needs.HH=="yes",
        int.wash.sub.living_standards.quantity.s3 = I.WASH.enough_drinking_and_one_other.HH=="yes"|I.WASH.enough_drinking_and_two_other.HH== "yes",
        int.wash.sub.living_standards.quantity.s4 = I.WASH.enough_drinking_only.HH=="yes",
        int.wash.sub.living_standards.quantity.s5 = enough_water.drinking==0,
        int.wash.sub.living_standards.quantity.s6 = enough_water.none_previous==1 ,
        
        int.wash.sub.living_standards.quantity=if_else(int.wash.sub.living_standards.quantity.s6==1,6,
                                                       if_else(int.wash.sub.living_standards.quantity.s5==1,5,
                                                               if_else(int.wash.sub.living_standards.quantity.s4==1,4,
                                                                       if_else(int.wash.sub.living_standards.quantity.s3==1, 3,
                                                                               if_else(int.wash.sub.living_standards.quantity.s1==1,1,99))))),
        
        int.wash.sub.living_standards.sanitation.s1 =defecation %in% c("shared_latrine", "hh_latrine", "dont_know"),
        int.wash.sub.living_standards.sanitation.s2= defecation =="pubic_latrine",
        int.wash.sub.living_standards.sanitation.s4 = defecation %in% c("bucket_connode","plastic_bag"),
        # int.wash.sub.living_standards.sanitation.s5 = defecation %in%  c("hanging_toilet","bucket"),
        int.wash.sub.living_standards.sanitation.s6 = defecation %in% c("open_defecation"),
        
        int.wash.sub.living_standards.sanitation=if_else(int.wash.sub.living_standards.sanitation.s6==1,6,
                                                         # if_else(int.wash.sub.living_standards.sanitation.s5==1,5,
                                                         if_else(int.wash.sub.living_standards.sanitation.s4==1, 4,
                                                                 if_else(int.wash.sub.living_standards.sanitation.s2==1,2,99))),
        
        # int.wash.sub.living_standards.soap= if_else(soap=="yes",2,
                                                    # if_else(soap=="no", 4,99)),
        
        
        
        vis.faeces= visible_faeces== "yes",
        vis.waste= visible_waste=="yes",
        vis.stagwater= stagnant_water=="yes")
    
    mode_int<-hh_data %>% 
      group_by(!!sym(strata)) %>% 
      summarise(mode.int.wash.sub.living_standards.quantity=getmode(int.wash.sub.living_standards.quantity))
    
    fixed_water_quantity<-hh_data[rows_to_fix,c("X_uuid","camp_name")] %>% left_join(mode_int, by="camp_name")
    
    hh_data$int.wash.sub.living_standards.quantity<-ifelse(hh_data$X_uuid %in%
                                                             fixed_water_quantity$X_uuid,
                                                           mode_int$mode.int.wash.sub.living_standards.quantity,
                                                           hh_data$int.wash.sub.living_standards.quantity) %>% as.numeric()
    
    living_standards_subs<-c("int.wash.sub.living_standards.water_source", "int.wash.sub.living_standards.quantity","int.wash.sub.living_standards.sanitation", "int.wash.sub.living_standards.environment")
    
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
          water_source_drink.cart_small_tank| water_source_drink.protected_dugwell|water_source_drink.tanker_truck|water_source_drink.pipe_water|water_source_drink.rainwater_collected,
        
        int.wash.improved_water_wash= water_source_wash.tubewells | water_source_wash.protected_spring|
          water_source_wash.cart_small_tank| water_source_wash.protected_dugwell|water_source_wash.tanker_truck|water_source_wash.pipe_water,
        
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
        
        
        
        int.wash.sub.living_standards.water_source.s4=(int.wash.improved_water_wash& (int.wash.improved_water_drinking|water_source_drink.pipe_water) ) & 
          int.wash.time_greater_30_min==1, 
        
        int.wash.sub.living_standards.water_source.s5= int.wash.unimproved_water_drinking,
        
        int.wash.sub.living_standards.water_source.s6 = water_source_drink.surface_water, #| water_source_wash.surface_water 
        
        int.wash.sub.living_standards.water_source=if_else(int.wash.sub.living_standards.water_source.s6==1,6,
                                                           if_else(int.wash.sub.living_standards.water_source.s5==1,5,
                                                                   if_else(int.wash.sub.living_standards.water_source.s4==1,4,
                                                                           if_else(int.wash.sub.living_standards.water_source.s3==1, 3,
                                                                                   if_else(int.wash.sub.living_standards.water_source.s2==1,2,99))))),
        
        
        sev_score.wash.sub.coping.water = if_else(surface_water_access %in% c("never", "dont_know"),1,
                                                  if_else(surface_water_access== "couple_times",3,
                                                          if_else(surface_water_access== "almost_everyday",5,99))),
        
        #BUILD THIS FUCKING WEIGHT
        index.wash.unsafe_male = if_else(feel_unsafe_male.latrine| feel_unsafe_male.water_points| feel_unsafe_male.bathing_areas| 
                                           feel_unsafe_male.way_to_facilities,2,
                                         if_else(feel_unsafe_male.market|feel_unsafe_male.market,1,0)),
        index.wash.unsafe_female=if_else(feel_unsafe_female.latrine| feel_unsafe_female.water_points| feel_unsafe_female.bathing_areas| 
                                           feel_unsafe_female.way_to_facilities,3,if_else(
                                             feel_unsafe_female.market|feel_unsafe_female.market,1,0)),
        
        index.wash.unsafe_male_reason= if_else(unsafe_reason_male.lack_of_light==1,2,
                                               if_else(unsafe_reason_male.intra_community_tension==1,1,0)),
        
        index.wash.unsafe_female_reason= if_else(unsafe_reason_female.lack_of_light==1,3,
                                                 if_else(unsafe_reason_female.intra_community_tension==1,1,0)),
        index.wash.women_market_access= if_else(unmarried_market_mobility=="never_alone" | 
                                                  married_market_mobility=="never_alone",4,
                                                if_else(unmarried_market_mobility!= "not_alone"|
                                                          married_market_mobility!= "not_alone",2,0)),
        index.wash.no_portable_lights=if_else(solar_light== 0, 3,0),
       
      )
    
    wash_indices<- c("index.wash.unsafe_male", "index.wash.unsafe_female", "index.wash.unsafe_male_reason", 
                     "index.wash.unsafe_female_reason", "index.wash.women_market_access", 
                     "index.wash.no_portable_lights")
    
    wash_combo_table<-wash_combo_table %>% 
      mutate(coping_wellbeing_combo=paste0(wash_coping, "_", wash_wellbeing))
    
    hh_data<-hh_data %>% 
      mutate(
        indices_summed.wash= rowSums(.[wash_indices],na.rm=TRUE),
        int.wash.sub.wellbeing.safety_index= if_else(indices_summed.wash<1,1,
                                                     if_else(indices_summed.wash <6,2,
                                                             if_else(indices_summed.wash<12,3,
                                                                     if_else(indices_summed.wash<19,4,
                                                                             if_else(indices_summed.wash<36,5,99))))),
        under5_diar=ifelse(is.na(under5_diarrhea),0,under5_diarrhea),
        
        int.wash.sub.wellbeing.diarh= if_else(under5_diar>0  & over5_diarrhea>0,5,
                                              if_else(under5_diar>0 & over5_diarrhea==0, 4,
                                                      if_else(under5_diar==0 & over5_diarrhea>0,3,1))))#,
        
    hh_data<-hh_data %>% 
      mutate(
        #LIVING STANDARDS CUMULATIVE
        living_standards_subs_rowsums= rowSums(.[living_standards_subs], na.rm=TRUE),
        living_standards_subs_rowsums5=rowSums(sapply(hh_data[,living_standards_subs], function(x) ifelse(x==5, TRUE,FALSE))),
        living_standards_subs_rowsums4=rowSums(sapply(hh_data[,living_standards_subs], function(x) ifelse(x==4, TRUE,FALSE))),
        living_standards_subs_rowsums3=rowSums(sapply(hh_data[,living_standards_subs], function(x) ifelse(x==3, TRUE,FALSE))),
        living_standards_subs_rowsums2=rowSums(sapply(hh_data[,living_standards_subs], function(x) ifelse(x==2, TRUE,FALSE))),
        living_standards_subs_rowsums1=rowSums(sapply(hh_data[,living_standards_subs], function(x) ifelse(x==1, TRUE,FALSE))),
        living_standards.s6<-if_else((int.wash.sub.living_standards.quantity==6 |
                                        int.wash.sub.living_standards.water_source==6 |int.wash.sub.living_standards.sanitation==6)|
                                       living_standards_subs_rowsums5>1,1,0),
        six_test=if_else(int.wash.sub.living_standards.quantity==6 |
                           int.wash.sub.living_standards.water_source==6 |int.wash.sub.living_standards.sanitation==6,1,0),
        five_test=if_else(living_standards_subs_rowsums5>1,1,0), 
        living_standards.s5<- if_else(living_standards_subs_rowsums5>0,1,0),
        living_standards.s4<- if_else(living_standards_subs_rowsums4>1,1,0),
        living_standards.s3<- if_else(living_standards_subs_rowsums3>1|living_standards_subs_rowsums4==1, 1,0),
        living_standards.s2<- if_else(living_standards_subs_rowsums2>1|living_standards_subs_rowsums3==1, 1,0, missing = 99),
        living_standards.s1<- if_else(living_standards_subs_rowsums1>0|living_standards_subs_rowsums2<2, 1,0, missing = 99),
        
        sev_score.living_standards.subtotal=if_else(living_standards.s6==1,6,
                                                    if_else(living_standards.s5==1,5,
                                                            if_else(living_standards.s4==1, 4,
                                                                    if_else(living_standards.s3==1, 3,
                                                                            if_else(living_standards.s2==1,2,
                                                                                    if_else(living_standards.s1==1,1,99)))))),
        sev_score.coping.subtotal=  sev_score.wash.sub.coping.water,
        sev_score.wellbeing.subtotal=  apply(hh_data[,c("int.wash.sub.wellbeing.safety_index","int.wash.sub.wellbeing.diarh")],1,max),
        coping_wellbeing_combo= paste0(sev_score.coping.subtotal,"_",sev_score.wellbeing.subtotal),
        
      )
    
    hh_data<-hh_data %>% 
      left_join(
        wash_combo_table %>% 
          select(coping_wellbeing_combo, Final), by= "coping_wellbeing_combo")
    
    hh_data<-hh_data %>% 
      mutate(
        sev_score.wash.total=ifelse(sev_score.living_standards.subtotal > sev_score.coping.subtotal|
                                      sev_score.living_standards.subtotal > sev_score.wellbeing.subtotal,sev_score.living_standards.subtotal,Final)
      )
    
    
  }
  hh_data
  }



