

recode_to_severity_bgd_msna2019<-name <- function(hh_data,individual_data, population) {
  
  if(population=="Host"){
    food_source_borrowed=sym("food_source.borrow")
    hh_coping_4=sym("hh_coping_mechanism.sell_assets")
    coping_sell_hh_goods=sym("hh_coping_mechanism.sell_goods")
    ind_why_notreatment_night_safety=sym("ind_why_notreatment.night_safety")
    hh_data<-hh_data %>% 
      mutate(
        coping_bought_on_credit= hh_coping_mechanism.bought_on_credit|food_source.purchase_credit,
        hh_coping_5=hh_coping_mechanism.adult_work_long_hr|hh_coping_mechanism.migration 
        
      )
  }else{
    coping_sell_hh_goods=sym("hh_coping_mechanism.sell_hhgoods")
    food_source_borrowed= sym( "food_source.borrowed")
    hh_coping_4 = sym("hh_coping_mechanism.migration")
    ind_why_notreatment_night_safety=sym("ind_why_notreatment.safety_concerns_night")
  
    hh_data<-hh_data %>% 
      mutate(
        coping_bought_on_credit=hh_coping_mechanism.bought_on_credit,
        hh_coping_5=hh_coping_mechanism.adult_work_long_hr
      )
    
  }
  
  # common intermediate variables -------------------------------------------
  building.blocks.snfi<-c("improvement_reason","debt_reason.repair_build_shelter","improvement.none","shelter_issues.none","shelter_purchased","improvement_reason") 
  
  HH_rec_step1<-hh_data%>% 
    mutate(
      #INTERMEDIATE INDICATORS
      #STRUCTURE
     
      int.snfi.debt_for_shelter= if_else(debt_reason.repair_build_shelter==TRUE,1,0, missing=0),
      int.snfi.improvement_made=if_else(improvement.none==1 | improvement.dntknow_prefer==1,0,1),
      

      #SECURITY OF TENURE
      int.snfi.debt_for_rent= if_else(debt_reason.pay_house_rent==1,1,0, missing=0),
      
      #HEALTH
      int.health.pay =  if_else(exp_medical != "0_bdt"| health_coping.pay_care,1,0,missing=0),
      int.health.debt= if_else(health_coping.ghealth_debt|debt_reason.pay_health_expenses ,1,0,missing=0),
      int.health.priority_needs= if_else(hh_priority_needs.access_health_facilities==1 ,1,0),
      int.health.wash_diar_under5= if_else(under5_diarrhea>0 ,1,0, missing=0),
      int.health.preg_anc = if_else(pregnant_woman=="yes" & pregnant_women_anc >0 ,1,0, missing=0),
      
      #COPING

      int.coping.spend_savings= income_source.savings|hh_coping_mechanism.spend_savings,
      int.coping.selling_hh_goods= income_source.sell_hh_item| !!sym(coping_sell_hh_goods)
    ) 
  HH_rec_step1 %>% select(starts_with("int.coping")) %>% colnames() %>% dput()
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
      int.health.smoking_everyday= if_else(ind_age>17 & ind_smoke=="everyday",1,0,missing = 0),
      int.health.smoking_everyday_somedays= if_else(ind_age>17 & ind_smoke %in% c("everyday","some_days"),1,0,missing = 0)
      
      
    )
  
  at_least_one <- function(variable) {
    if_else(sum(variable,na.rm = TRUE)>0,1,0)
    
  }
  all_fam <- function(variable) {
    if_else(mean(variable,na.rm = TRUE)==1,1,0)
    
  }
  
  indiv_to_indiv %>% 
    group_by(X_submission__uuid) %>% 
    summarise(
      int.health.sick_no_treatment_atleast= at_least_one(int.health.sick_no_treatment),
      int.health.sick_gov_ngo_treatment_atleast= at_least_one(int.health.sick_gov_ngo_treatment),
      int.health.sick_pharmacy_or_traditional_atleast= at_least_one(int.health.sick_pharmacy_or_traditional),
      int.health.sick_private_atleast= at_least_one(int.health.sick_private),
      int.health.sick_no_treatment_not_safety_atleast= at_least_one(int.health.sick_no_treatment_not_safety),
      int.health.sick_no_treatment_safety_atleast= at_least_one(int.health.sick_no_treatment_safety),
      int.health.birthed_not_clinic_atleast= at_least_one(int.health.birthed_not_clinic),
      #ALL
      int.health.adult_requiring_assistance_all= all_fam(int.health.adult_requiring_assistance),
      int.health.smoking_everyday_all= all_fam(int.health.smoking_everyday)
      
      
    )
  
  
  # Refugee Only ------------------------------------------------------------
  
  
  cooking_fuel_non_wood_options<-c(
    "cooking_fuel.lpg_gas_cylinder", "cooking_fuel.kerosene_stove", 
    "cooking_fuel.dung_cakes", "cooking_fuel.other", "cooking_fuel.dntknow_prefer"
  )
  
  
  if(population=="Refugee"){
    HH_rec_step2<-HH_rec_step1 %>% 
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
        sev_score.snfi.structure= if_else(sev.snfi.structure.s4==1,4,
                                          if_else(sev.snfi.structure.s3==1,3,
                                                  if_else(sev.snfi.structure.s2==1,2,
                                                          if_else(sev.snfi.structure.s1==1,1, 99)))),
        int.snfi.tenure.rent_6month_paid=if_else(shelter_paid=="yes",1, 0),
        sev_score.snfi.tenure=if_else((int.snfi.tenure.rent_6month_paid==1 & int.snfi.debt_for_rent==1),4,if_else(int.snfi.tenure.rent_6month_paid==1,3,1)),
        #power
        int.snfi.power.lpg_only = I.SNFI.lpg_only.HH=="yes",
        int.snfi.power.wood= cooking_fuel.collected_firewood | cooking_fuel.purchased_firewood,
        int.snfi.power.wood_only= if_else((int.snfi.power.wood) & rowSums(.[cooking_fuel_non_wood_options], na.rm=T)==0,1,0),
        sev.snfi.power.s2=int.snfi.power.lpg_only,
        sev.snfi.power.s3=int.snfi.power.wood|cooking_fuel.dung_cakes|cooking_fuel.kerosene_stove|cooking_fuel.other,
        sev.snfi.power.s4=int.snfi.power.wood_only,
        sev_score.snfi.power= if_else(sev.snfi.power.s4==1,4,
                                      if_else(sev.snfi.power.s3==1,3,
                                              if_else(sev.snfi.power.s2==1,2,99))),
        int.snfi.nfi.min_blanket=I.SNFI.min_blanket.HH=="yes",
        int.snfi.nfi.min_floormat=I.SNFI.min_floormat.HH=="yes",
        int.snfi.nfi.min_light = (hh_size==1  & portable_light>0) | (hh_size>1 & portable_light>1),
        int.snfi.nfi.no_lights = portable_light==0,
        int.snfi.nfi.one_light = portable_light==1 & hh_size>1,
        int.snfi.nfi.blankets_half= if_else(blanket> hh_size/2, TRUE, FALSE),
        int.snfi.nfi.floormat_half= if_else(floormat> hh_size/2, TRUE, FALSE),
        sev.snfi.nfi.s2 = int.snfi.nfi.min_blanket & int.snfi.nfi.min_floormat & int.snfi.nfi.min_light,
        sev.snfi.nfi.s3 = int.snfi.nfi.one_light | (int.snfi.nfi.min_blanket==FALSE|int.snfi.nfi.min_floormat==FALSE ),
        sev.snfi.nfi.s4 = (int.snfi.nfi.blankets_half | int.snfi.nfi.floormat_half) & int.snfi.nfi.no_lights ,
        sev_score.snfi.nfi= if_else(sev.snfi.nfi.s4==1,4,
                                    if_else(sev.snfi.nfi.s3==1,3,
                                            if_else(sev.snfi.nfi.s2==1,2,99))),
        #HEALTH
        int.health.comm_health_worker= if_else(comm_health_worker=="yes",1,0),
        
        # COPING
        
        int.coping.borrowed_goods= hh_coping_mechanism.borrowed| food_source.borrowed,
        
        sev.coping.s1= hh_coping_mechanism.none ,
        sev.coping.s2= hh_coping_mechanism.sell_assist |hh_coping_mechanism.sell_food,
        
        sev.coping.s3=int.coping.spend_savings==TRUE|int.coping.selling_hh_goods==1|int.coping.borrowed_goods==1| hh_coping_mechanism.sell_jewlery|
          hh_coping_mechanism.reduce_expenditure|hh_coping_mechanism.reduce_agricultural_expense==1| hh_coping_mechanism.sell_labour==1|
          coping_bought_on_credit==1,
        
        sev.coping.s4= hh_coping_mechanism.sell_firewood | hh_coping_mechanism.dependent_neighbour_ration|!!sym(hh_coping_4),
        sev.coping.s5=hh_coping_5| hh_coping_mechanism.withdraw_child_school|
          hh_coping_mechanism.child_marriage|hh_coping_mechanism.inter_marriage|
          hh_coping_mechanism.begging|hh_coping_mechanism.accept_illegal_job
        
        
        #WASH
        
        
      ) 
    
    snfi_severity_score_col_names<-c("sev_score.snfi.structure", "sev_score.snfi.tenure", "sev_score.snfi.power", 
                                     "sev_score.snfi.nfi")

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
    HH_rec_step1$shelter_typology<-calculate_shelter_typology(HH_rec_step1)
    HH_rec_step2<-HH_rec_step1 %>% 
      mutate(
        #STRUCTURE
  
        int.snfi.improvement_not_needed=if_else(improvement_reason.no_need,1,0,missing=0),
        int.snfi.improvement_unable=if_else(improvement_reason.no_need,0,1,missing=0),
        sev.snfi.structure.s1= int.snfi.improvement_not_needed==1 |(int.snfi.improvement_made==1 &int.snfi.debt_for_shelter==0), 
        sev.snfi.structure.s3= (int.snfi.improvement_made==1 & int.snfi.debt_for_shelter==1)| shelter_typology=="Jhuprie", 
        sev.snfi.structure.s4 = int.snfi.improvement_unable,
        sev_score.snfi.structure=if_else(sev.snfi.structure.s4==1,4,
                                         if_else(sev.snfi.structure.s3==1,3,
                                                 if_else(sev.snfi.structure.s1==1,1, 99))),
        #TENURE
        #intermediate
        int.snfi.tenure.own_house=if_else(house %in% c("yes_own_house" ,"yes_coown" ), 1, 0),
        int.snfi.tenure.pay_rent=if_else(rent_host=="pay_rent",1,0, missing=0),
        int.snfi.tenure.no_written_agreement=if_else(agreement=="no", 1, 0, missing=0),
        int.snfi.tenure.written_agreement=if_else(agreement=="yes", 1, 0, missing=0),
        #binary severities
        sev.snfi.tenure.s1= int.snfi.tenure.own_house, 
        sev.snfi.tenure.s2= int.snfi.tenure.written_agreement,
        sev.snfi.tenure.s3= (int.snfi.tenure.pay_rent ==1 & int.snfi.debt_for_rent==1)|int.snfi.tenure.no_written_agreement==1,
        sev.snfi.tenure.s4= int.snfi.tenure.pay_rent ==1 & int.snfi.debt_for_rent==1 & int.snfi.tenure.no_written_agreement==1,
        sev.snfi.tenure.s5= house_eviction=="yes",
        
        sev_score.snfi.tenure=if_else(sev.snfi.tenure.s5==1,5,
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
        #what about no_grid.not_installed?
        sev.snfi.power.s3= (int.snfi.power.grid_connected==0 & int.snfi.power.debt_for_electricity==1)| (int.snfi.power.grid_cant_afford==1| int.snfi.power.grid_not_installed==1),
        sev.snfi.power.s5= (int.snfi.power.grid_connected==0 |  int.snfi.power.no_electricity_at_night==1)& int.snfi.power.no_portable_light==1 ,
        sev_score.snfi.power=if_else(sev.snfi.power.s5==1,5,
                                     if_else(sev.snfi.power.s3==1,3,
                                             if_else(sev.snfi.power.s1==1,1, 99))),
        #COPING
        int.coping.bought_items_on_credit=food_source.purchase_credit,
  
        int.coping.borrowed_goods=   hh_coping_mechanism.borrowed| food_source.borrow,
        
        sev.coping.s1= hh_coping_mechanism.none ,
      
        sev.coping.s2= hh_coping_mechanism.sell_assist |hh_coping_mechanism.sell_food,
        
        sev.coping.s3=int.coping.spend_savings|int.coping.selling_hh_goods|int.coping.borrowed_goods| hh_coping_mechanism.sell_jewlery|
          hh_coping_mechanism.reduce_expenditure|hh_coping_mechanism.reduce_agricultural_expense| hh_coping_mechanism.sell_labour|
          coping_bought_on_credit,
        
        sev.coping.s4= hh_coping_mechanism.sell_firewood | hh_coping_mechanism.dependent_neighbour_ration|!!sym(hh_coping_4),
        sev.coping.s5=hh_coping_5| hh_coping_mechanism.withdraw_child_school|
          hh_coping_mechanism.child_marriage|hh_coping_mechanism.inter_marriage|
          hh_coping_mechanism.begging|hh_coping_mechanism.accept_illegal_job
      ) 
    
    
    snfi_severity_score_col_names<-c("sev_score.snfi.structure", "sev_score.snfi.tenure", "sev_score.snfi.power")
    
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

  
  
}
