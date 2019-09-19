
# recode_COPING_severity_bgd2019(hh_data=host, individual_data=host_indiv, population="Host")
# recode_COPING_severity_bgd2019(hh_data=ref, individual_data=ref_indiv, population="Refugee")

# host$food_source.purchase_credit
# ref$food_source.purchase
# ref$hh_coping_mechanism.spend_savings
# host$hh_coping_mechanism.spend_savings
# host$hh_coping_mechanism.borrowed
# ref$hh_coping_mechanism.sell_assist
# host$hh_coping_mechanism.sell_nfi 
# # 

recode_COPING_severity_bgd2019<-name <- function(hh_data,individual_data, population) {
  
  if(population=="Host"){
    food_source_borrowed=sym("food_source.borrow")
    hh_coping_sell_assistance=sym("hh_coping_mechanism.sell_nfi")
    hh_coping_sell_jewelry=sym("hh_coping_mechanism.sell_gold")
    hh_coping_sell_food= sym("hh_coping_mechanism.exchange_sell_food")
    hh_coping_mechanism_migration=sym("hh_coping_mechanism.sell_assets")
    coping_sell_hh_goods=sym("hh_coping_mechanism.sell_goods")
    ind_why_notreatment_night_safety=sym("ind_why_notreatment.night_safety")
    hh_data<-hh_data %>% 
      mutate(
        coping_bought_on_credit= hh_coping_mechanism.bought_on_credit|food_source.purchase_credit,
        hh_coping_5=hh_coping_mechanism.adult_work_long_hr|hh_coping_mechanism.migration,
        int.coping.borrowed_goods=   hh_coping_mechanism.borrowed==1| food_source_borrowed==1
        
      )
  }else{
    food_source_borrowed= sym("food_source.borrowed")
    
    coping_sell_hh_goods=sym("hh_coping_mechanism.sell_hhgoods")
    hh_coping_sell_assistance= sym("hh_coping_mechanism.sell_assist")
    hh_coping_sell_food= sym("hh_coping_mechanism.sell_food")
    hh_coping_sell_jewelry=sym("hh_coping_mechanism.sell_jewlery")
    hh_coping_mechanism_migration = sym("hh_coping_mechanism.migration")
    ind_why_notreatment_night_safety=sym("ind_why_notreatment.safety_concerns_night")
    
    hh_data<-hh_data %>% 
      mutate(
        coping_bought_on_credit=hh_coping_mechanism.bought_on_credit,
        hh_coping_5=hh_coping_mechanism.adult_work_long_hr,
        int.coping.borrowed_goods=   hh_coping_mechanism.borrowed| food_source_borrowed==1|income_source.borrowed
      )
    
  }
  
  # common intermediate variables -------------------------------------------
  HH_rec_step1<-hh_data%>% 
    mutate(
      #COPING
      int.coping.spend_savings= income_source.savings|hh_coping_mechanism.spend_savings,
      int.coping.selling_hh_goods= income_source.sell_hh_item| !!sym(coping_sell_hh_goods),
      
  
      

      
      sev.coping.s1= hh_coping_mechanism.none ,
      sev.coping.s2= !!sym(hh_coping_sell_assistance) |!!sym(hh_coping_sell_food),
      sev.coping.s3=int.coping.spend_savings|int.coping.selling_hh_goods|int.coping.borrowed_goods| !!sym(hh_coping_sell_jewelry)|
        hh_coping_mechanism.reduce_expenditure|hh_coping_mechanism.reduce_agricultural_expense| hh_coping_mechanism.sell_labour|
        coping_bought_on_credit,
      
      
      sev.coping.s4= hh_coping_mechanism.sell_firewood | hh_coping_mechanism.dependent_neighbour_ration|!!sym(hh_coping_mechanism_migration),
      sev.coping.s5=hh_coping_5| hh_coping_mechanism.withdraw_child_school|
        hh_coping_mechanism.child_marriage|hh_coping_mechanism.inter_marriage|
        hh_coping_mechanism.begging|hh_coping_mechanism.accept_illegal_job
    
    ) 

  # Individual intermediate aggregations ------------------------------------
  indiv<-individual_data
  indiv_to_indiv<-indiv %>% 
    mutate(
      
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
      
    )
  
  
  # Refugee Only ------------------------------------------------------------
  

  
  if(population=="Refugee"){
    HH_rec_step2<-HH_rec_step1 %>% 
      mutate(

        # int.coping.borrowed_goods=   hh_coping_mechanism.borrowed| food_source_borrowed|income_source.borrowed

      ) 
    

    
    HH_rec_step3<-HH_rec_step2 %>% 
      mutate(

      ) 
    
  }
  
  if(population=="Host"){
    HH_rec_step2<-HH_rec_step1 %>% 
      mutate(
        # int.coping.borrowed_goods=   hh_coping_mechanism.borrowed| food_source_borrowed
      ) 
    
    

    HH_rec_step3<-HH_rec_step2 %>% 
      mutate(
      ) 
    
  }
  
  
  
}

