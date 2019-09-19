host_hh<-HH_yes_consent
host_indiv<-indiv_yes_consent

ref_hh<-HH_yes_consent
ref_indiv<-indiv_yes_consent

hh_data<-ref_hh
individual_data<-ref_indiv

# indiv_data<-host_indiv
recode_SNFI_severity_bgd2019<-name <- function(hh_data,individual_data, population) {
  

  
  # common intermediate variables -------------------------------------------
  building.blocks.snfi<-c("improvement_reason","debt_reason.repair_build_shelter","improvement.none","shelter_issues.none","shelter_purchased","improvement_reason") 
  
  HH_rec_step1<-hh_data%>% 
    mutate(
      #STRUCTURE
     
      int.snfi.debt_for_shelter= if_else(debt_reason.repair_build_shelter==TRUE,1,0, missing=0),
      int.snfi.improvement_made=if_else(improvement.none==1 | improvement.dntknow_prefer==1,0,1),
      

      #SECURITY OF TENURE
      int.snfi.debt_for_rent= if_else(debt_reason.pay_house_rent==1,1,0, missing=0),
      
    ) 
  
    # Individual intermediate aggregations ------------------------------------

  
  # Refugee Only ------------------------------------------------------------
  
  
  cooking_fuel_non_wood_options<-c(
    "cooking_fuel.lpg_gas_cylinder", "cooking_fuel.kerosene_stove", 
    "cooking_fuel.dung_cakes", "cooking_fuel.other", "cooking_fuel.dntknow_prefer"
  )
  
  
  if(population=="Refugee"){
    
    c("cooking_fuel.purchased_firewood", "cooking_fuel.collected_firewood", 
      "cooking_fuel.lpg_gas_cylinder", "cooking_fuel.kerosene_stove", 
      "cooking_fuel.dung_cakes", "cooking_fuel.other", "cooking_fuel.dntknow_prefer")
    
    HH_rec_step2<- HH_rec_step1 %>% 
      mutate(
        int.snfi.improvement_not_needed=if_else(improvement_reason=="no_need",1,0,missing = 1),
        int.snfi.improvement_unable=if_else(improvement_reason=="no_need",0,1,missing = 0),
        int.snfi.faced_issues=if_else(shelter_issues.none==1,0,1),
        int.snfi.purchased_materials= if_else(shelter_purchased== "yes",1,0, missing=0),
        sev.snfi.structure.s1=int.snfi.improvement_not_needed,
        sev.snfi.structure.s2=int.snfi.improvement_made==1 & int.snfi.purchased_materials==0,
        sev.snfi.structure.s3=int.snfi.improvement_made==1 & int.snfi.purchased_materials==1 & int.snfi.debt_for_shelter==0,
        sev.snfi.structure.s4=(int.snfi.improvement_made==1 & int.snfi.purchased_materials==1 & int.snfi.debt_for_shelter==1)|int.snfi.improvement_unable==1,
        # 
        sev_score.snfi.sub.structure= if_else(sev.snfi.structure.s4==1,4,
                                          if_else(sev.snfi.structure.s3==1,3,
                                                  if_else(sev.snfi.structure.s2==1,2,
                                                          if_else(sev.snfi.structure.s1==1,1, 99)))),
        int.snfi.tenure.rent_6month_paid=if_else(shelter_paid=="yes",1, 0),
        sev_score.snfi.sub.tenure=if_else((int.snfi.tenure.rent_6month_paid==1 & int.snfi.debt_for_rent==1),4,
                                          if_else(int.snfi.tenure.rent_6month_paid==1,3,2)),
        #fuel
        # int.snfi.fuel.lpg_only = I.SNFI.lpg_only.HH=="yes",
        # int.snfi.fuel.wood= cooking_fuel.collected_firewood | cooking_fuel.purchased_firewood,
        # int.snfi.fuel.wood_only= if_else((int.snfi.fuel.wood) & rowSums(.[cooking_fuel_non_wood_options], na.rm=T)==0,1,0)),
        sev.snfi.fuel.s2= (cooking_fuel.lpg_gas_cylinder|cooking_fuel.kerosene_stove) &
          cooking_fuel.purchased_firewood==FALSE &
          cooking_fuel.collected_firewood== FALSE & cooking_fuel.dung_cakes==FALSE & cooking_fuel.other==FALSE & cooking_fuel.dntknow_prefer==FALSE,
        
        
        sev.snfi.fuel.s3= (cooking_fuel.lpg_gas_cylinder|cooking_fuel.kerosene_stove) & (cooking_fuel.purchased_firewood==TRUE |
          cooking_fuel.collected_firewood==TRUE| cooking_fuel.dung_cakes==TRUE | cooking_fuel.other==TRUE | cooking_fuel.dntknow_prefer==TRUE),
        
        sev.snfi.fuel.s4= (cooking_fuel.lpg_gas_cylinder==FALSE & cooking_fuel.kerosene_stove==FALSE),
        
        sev_score.snfi.sub.fuel= if_else(sev.snfi.fuel.s4==1,4,
                                      if_else(sev.snfi.fuel.s3==1,3,
                                              if_else(sev.snfi.fuel.s2==1,2,99))),
        # int.snfi.power.min_blanket=I.SNFI.min_blanket.HH=="yes",
        # int.snfi.power.min_floormat=I.SNFI.min_floormat.HH=="yes",
        int.snfi.power.min_light = (hh_size==1  & portable_light>0) | (hh_size>1 & portable_light>1),
        int.snfi.power.no_lights = portable_light==0,
        int.snfi.power.one_light = portable_light==1 & hh_size>1,
        # int.snfi.power.blankets_half= if_else(blanket> hh_size/2, TRUE, FALSE),
        # int.snfi.power.floormat_half= if_else(floormat> hh_size/2, TRUE, FALSE),
        sev.snfi.power.s2 =  int.snfi.power.min_light,
        sev.snfi.power.s3 = int.snfi.power.one_light ,
        sev.snfi.power.s4 =  int.snfi.power.no_lights ,
        sev_score.snfi.sub.power= if_else(sev.snfi.power.s4==1,4,
                                    if_else(sev.snfi.power.s3==1,3,
                                            if_else(sev.snfi.power.s2==1,2,99))),
      
      ) 
    HH_rec_step2 %>% group_by(sev_score.snfi.sub.power) %>% count()
    snfi_severity_score_col_names<-c("sev_score.snfi.sub.structure", "sev_score.snfi.sub.tenure", "sev_score.snfi.sub.power", 
                                     "sev_score.snfi.sub.fuel")
    HH_rec_step3<-HH_rec_step2 %>% 
      mutate(
        sev_score.snfi.number4=rowSums(sapply(HH_rec_step2[,snfi_severity_score_col_names], function(x) ifelse(x==4, TRUE,FALSE))),
        sev_score.snfi.number3=rowSums(sapply(HH_rec_step2[,snfi_severity_score_col_names], function(x) ifelse(x==3, TRUE,FALSE))),
        sev_score.snfi.number2=rowSums(sapply(HH_rec_step2[,snfi_severity_score_col_names], function(x) ifelse(x==2, TRUE,FALSE))),
        sev_score.snfi.number1=  rowSums(sapply(HH_rec_step2[,snfi_severity_score_col_names], function(x) ifelse(x==1, TRUE,FALSE))),
        sev_score.snfi.total.s5=if_else(sev_score.snfi.number4==4,1,0),
        sev_score.snfi.total.s4=if_else((sev_score.snfi.number4>1 & sev_score.snfi.number3>0)|sev_score.snfi.number3==4,  1,0),
        sev_score.snfi.total.s3=sev_score.snfi.number3>1,
        sev_score.snfi.total.s2=sev_score.snfi.number2>0,
        sev_score.snfi.total=if_else(sev_score.snfi.total.s5==1,5,
                                     if_else(sev_score.snfi.total.s4==1,4,
                                             if_else(sev_score.snfi.total.s3==1,3,
                                                     if_else(sev_score.snfi.total.s2==1, 2,1))))
      ) 
    
  }

  if(population=="Host"){

    sustainable_income_sources<-c("income_source.employment", "income_source.agricultural_production_sale", 
                                 "income_source.gather_sell_firewood", "income_source.social_safety_net")
    
    unsustainable_income_sources<-c( "income_source.remittances", 
                                     "income_source.assistance_relative_friends", 
                                     "income_source.sell_hh_item", "income_source.savings", "income_source.begging", 
                                     "income_source.zakat", "income_source.other")
    


    HH_rec_step2<-HH_rec_step1 %>% 
      mutate(
        #STRUCTURE
        int.snfi.improvement_not_needed=if_else(improvement_reason.no_need==1,1,0,missing=0),
        int.snfi.improvement_unable=if_else(improvement_reason.no_need==1,0,1,missing=0),
        int.snfi.income_sustainable_rowsum= rowSums(.[sustainable_income_sources],na.rm=TRUE),
        int.snfi.income_unsustainable_rowsum= rowSums(.[unsustainable_income_sources],na.rm=TRUE),
        sev.snfi.structure.s1= int.snfi.improvement_not_needed==1 |(int.snfi.improvement_made==1 &int.snfi.debt_for_shelter==0), 
        sev.snfi.structure.s3= (int.snfi.improvement_made==1 & int.snfi.debt_for_shelter==1)| shelter_typology=="Jhuprie"|
          (int.snfi.improvement_unable & shelter_typology %in% c("SemiPucca", "Pucca")),
        sev.snfi.structure.s4 = int.snfi.improvement_unable & shelter_typology %in% c("Jhuprie", "Kutcha"),
        sev_score.snfi.sub.structure=if_else(sev.snfi.structure.s4==TRUE,4,
                                         if_else(sev.snfi.structure.s3==TRUE,3,
                                                 if_else(sev.snfi.structure.s1==TRUE,1, 1,99))),
        #TENURE
        #intermediate
        int.snfi.tenure.own_house=if_else(house %in% c("yes_own_house" ,"yes_coown" ), 1, 0),
        int.snfi.tenure.pay_rent=if_else(rent_host=="pay_rent",1,0, missing=0),
        int.snfi.tenure.no_written_agreement=if_else(agreement=="no", 1, 0, missing=0),
        int.snfi.tenure.written_agreement=if_else(agreement=="yes", 1, 0, missing=0),
        #binary severities
        sev.snfi.tenure.s1= int.snfi.tenure.own_house|(int.snfi.tenure.pay_rent==1 & int.snfi.income_sustainable_rowsum>0),
        sev.snfi.tenure.s2= int.snfi.tenure.written_agreement==1,
        sev.snfi.tenure.s3= (int.snfi.tenure.pay_rent ==1 & int.snfi.debt_for_rent==1)|int.snfi.tenure.no_written_agreement==1,
        sev.snfi.tenure.s4= int.snfi.tenure.pay_rent ==1 & int.snfi.debt_for_rent==1 & int.snfi.tenure.no_written_agreement==1&  int.snfi.income_sustainable_rowsum>0,
        
        sev.snfi.tenure.s5= int.snfi.tenure.pay_rent ==1 & int.snfi.debt_for_rent==1 & 
          int.snfi.tenure.no_written_agreement==1&  int.snfi.income_unsustainable_rowsum>0& int.snfi.income_sustainable_rowsum==0,  
        
        sev_score.snfi.sub.tenure=if_else(sev.snfi.tenure.s5==1,5,
                                      if_else(sev.snfi.tenure.s4==1,4,
                                              if_else(sev.snfi.tenure.s3==1,3,
                                                      if_else(sev.snfi.tenure.s2==1,2,
                                                              if_else(sev.snfi.tenure.s1==1,1, 99))))),
        #POWER
        int.snfi.power.grid_connected= electricity_grid=="yes",
        int.snfi.power.grid_not_needed= if_else(no_grid.not_needed==1,1,0,missing = 0),
        int.snfi.power.grid_not_installed=if_else(no_grid.grid_not_installed==1,1,0,missing = 0),
        int.snfi.power.debt_for_electricity= if_else(debt_reason.pay_electricity==1,1,0,missing=0),
        int.snfi.power.grid_cant_afford= if_else(no_grid.cannot_afford==1,1,0,missing=0),
        int.snfi.power.no_portable_light = if_else(solar_light==0,1,0),
        int.snfi.power.no_electricity_at_night = if_else(electricity_night=="no_nights",1,0,missing=0),
        sev.snfi.power.s1= int.snfi.power.grid_connected==1 | (int.snfi.power.grid_connected==0 & int.snfi.power.grid_not_needed==1),
        
        sev.snfi.power.s3= (int.snfi.power.grid_cant_afford==1| int.snfi.power.grid_not_installed==1),
        sev.snfi.power.s5= (int.snfi.power.grid_connected==0 |  int.snfi.power.no_electricity_at_night==1)& int.snfi.power.no_portable_light==1 ,
        sev_score.snfi.sub.power=if_else(sev.snfi.power.s5==1,5,
                                     if_else(sev.snfi.power.s3==1,3,
                                             if_else(sev.snfi.power.s1==1,1, 99)))

      ) 

    snfi_severity_score_col_names_obj<-HH_rec_step2[,c("sev_score.snfi.sub.structure", "sev_score.snfi.sub.tenure", "sev_score.snfi.sub.power")]
    snfi_severity_score_col_names<-c("sev_score.snfi.sub.structure", "sev_score.snfi.sub.tenure", "sev_score.snfi.sub.power")
##########NEED TO FIX
    HH_rec_step3<-HH_rec_step2 %>%
      mutate(
        sev_score.snfi.number4=rowSums(sapply(HH_rec_step2[,snfi_severity_score_col_names], function(x) ifelse(x==4, TRUE,FALSE))),
        sev_score.snfi.number4_over3= if_else(sev_score.snfi.number4>2,TRUE,FALSE),
      ) %>% 
      rowwise() %>% 
      mutate(sev_score.snfi.max= max(snfi_severity_score_col_names_obj)) 
      
    HH_rec_step3<-HH_rec_step3 %>% 
        mutate(sev_score.snfi.total=if_else(sev_score.snfi.number4_over3==TRUE,5,sev_score.snfi.max))
      
      HH_rec_step3 %>% group_by(sev_score.snfi.max) %>% count()
      # mutate(
      #   sev_score.snfi.total=rowMa
      #   
      # 
      #   sev_score.snfi.number5=rowSums(sapply(HH_rec_step2[,snfi_severity_score_col_names], function(x) ifelse(x==5, TRUE,FALSE))),
      #   sev_score.snfi.number4=rowSums(sapply(HH_rec_step2[,snfi_severity_score_col_names], function(x) ifelse(x==4, TRUE,FALSE))),
      #   sev_score.snfi.number3=rowSums(sapply(HH_rec_step2[,snfi_severity_score_col_names], function(x) ifelse(x==3, TRUE,FALSE))),
      #   sev_score.snfi.number2=rowSums(sapply(HH_rec_step2[,snfi_severity_score_col_names], function(x) ifelse(x==2, TRUE,FALSE))),
      #   sev_score.snfi.number1=  rowSums(sapply(HH_rec_step2[,snfi_severity_score_col_names], function(x) ifelse(x==1, TRUE,FALSE))),
      #   sev_score.snfi.total.s5=if_else(sev_score.snfi.number5>1,1,0),
      #   sev_score.snfi.total.s4=if_else(sev_score.snfi.number5==1 | sev_score.snfi.number4>0,1,0),
      #   sev_score.snfi.total.s3=sev_score.snfi.number3>0,
      #   sev_score.snfi.total.s2=sev_score.snfi.number2>1,
      #   sev_score.snfi.total=if_else(sev_score.snfi.total.s5==1,5,
      #                                if_else(sev_score.snfi.total.s4==1,4,
      #                                        if_else(sev_score.snfi.total.s3==1,3,
      #                                                if_else(sev_score.snfi.total.s2==1, 2,1)))),
      #   sev_score.snfi.total_over_3= if_else(sev_score.snfi.total>3,1,0)
      # ) 
    
  }
  HH_rec_step3
  
}
