
# hh_data<-HH_severity
# sectoral_scores<-all_sectoral_severity_scores
# coping_strategy_score<- "sev_score.coping.total"

calculate_INTERSECTORAL_severity_bgd_msna2019<-function(hh_data,sectoral_scores,coping_strategy_score) {
  sectoral_scores_without_coping<-setdiff(sectoral_scores, coping_strategy_score)
  coping_score<-hh_data[,coping_strategy_score]
  hh_data<-hh_data %>% 
    mutate(
      number_6=rowSums(sapply(hh_data[,sectoral_scores], function(x) ifelse(x==6, TRUE,FALSE))),
      number_5=rowSums(sapply(hh_data[,sectoral_scores], function(x) ifelse(x==5, TRUE,FALSE))),
      number_4=rowSums(sapply(hh_data[,sectoral_scores_without_coping], function(x) ifelse(x==4, TRUE,FALSE))),
      number_3=rowSums(sapply(hh_data[,sectoral_scores_without_coping], function(x) ifelse(x==3, TRUE,FALSE))),
      number_2=rowSums(sapply(hh_data[,sectoral_scores_without_coping], function(x) ifelse(x==2, TRUE,FALSE))),
      number_1=rowSums(sapply(hh_data[,sectoral_scores_without_coping], function(x) ifelse(x==1, TRUE,FALSE))),
    
  
      intersectoral_severity.total=if_else(number_6>0,6,
                    if_else(number_5>0,5,
                            if_else(number_4>=2|coping_score==4,4,
                                    if_else(number_3>=2| coping_score==3,3,
                                            if_else(number_2>1|coping_score==2,1,1)))))
    )
  
  
}


