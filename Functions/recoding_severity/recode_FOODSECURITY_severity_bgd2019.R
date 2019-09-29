# hh_data<-HH_with_composite
# individual_data<- indiv_yes_consent

# host_indiv<- indiv_yes_consent
recode_FOODSECURITY_severity_bgd2019<- function(hh_data,individual_data, population) {
  food_types<-c("cereals_tubers", 
                "pulses_nuts_seeds",
                "vegetables",
                "fruits",
                "dairy",
                "meat_fish",
                "oil_fats",
                "sweets",
                "spices_condiments")
  
  
  if (population=="Host"){
    unsustainable_food_sources<-c("food_source.army_distributing","food_source.food_assistance_food_card")}
  if (population=="Refugee"){ 
    unsustainable_food_sources<-c("food_source.army", "food_source.assistance_distribution", "food_source.assistance_voucher")
  }
  
  
  hh_data<- hh_data %>% 
    mutate(
      int.FS.fcs_classified= if_else(I.FCS_score>42, "acceptable", 
                                     if_else(I.FCS_score>28, "borderline",
                                             if_else(I.FCS_score>21,"poor",
                                                     "very_poor"))),
      # HDDS=rowSums(sapply(hh_data[,food_types], function(x) ifelse(x==7, TRUE,FALSE))),
      HDDS_over5=rowSums(sapply(hh_data[,food_types], function(x) ifelse(x>5, TRUE,FALSE))),
      numfoods= rowSums(.[food_types], na.rm=TRUE),
      int.FS.HDDS_classified = if_else(HDDS_over5>=4, "acceptable",
                                       if_else(HDDS_over5>=3,"borderline",
                                               if_else(HDDS_over5>=2, "poor", "very_poor"))),
      unsustainable_food_source_rowsum= rowSums(.[unsustainable_food_sources], na.rm=TRUE),
      FCS_HDDS_both_acceptable= int.FS.fcs_classified== "acceptable" &int.FS.HDDS_classified=="acceptable",
      
      # sev_score.FS.total=if_else(FCS_HDDS_both_acceptable & unsustainable_food_source_rowsum==0,1,
      #                            if_else(FCS_HDDS_both_acceptable & unsustainable_food_source_rowsum>0,2,
      #                                    if_else(int.FS.HDDS_classified=="borderline"|int.FS.fcs_classified=="borderline",3,
      #                                            if_else(int.FS.HDDS_classified=="poor"|int.FS.fcs_classified=="poor",4,
      #                                                    if_else(int.FS.HDDS_classified %in% c("poor", "very_poor") & int.FS.fcs_classified=="poor",5,
      #                                                            if_else(int.FS.fcs_classified == "very_poor" &int.FS.HDDS_classified %in% c("poor", "very_poor"),6,99 ))))))
      
      sev_score.FS.total= if_else(int.FS.fcs_classified == "very_poor" &int.FS.HDDS_classified== "very_poor",6,
                                  if_else(int.FS.HDDS_classified == "very_poor" | int.FS.fcs_classified =="very_poor",5,
                                          if_else(int.FS.HDDS_classified=="poor"|int.FS.fcs_classified=="poor",4,
                                                  if_else(int.FS.HDDS_classified=="borderline"|int.FS.fcs_classified=="borderline",3,
                                                          if_else(FCS_HDDS_both_acceptable & unsustainable_food_source_rowsum>0,2,
                                                                  if_else(FCS_HDDS_both_acceptable & unsustainable_food_source_rowsum==0,1,99))))))
                            
                                        
                                                 
                                                         
                                                                
      
      
    )
}


