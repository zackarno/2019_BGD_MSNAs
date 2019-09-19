

kobo_factorize<-function(data, questionnaire){
  which_are_select_multiple<-which(
    sapply(names(data), questionnaire$question_is_select_multiple)
  )
  if(length(which_are_select_multiple)!=0){
    data<-data[,-which_are_select_multiple]
  }
  categorical_variables<-which(
    sapply(names(data), questionnaire$question_is_categorical)
  )
  categorical_data<-data[,categorical_variables]

  categorical_levels<-sapply(names(categorical_data),questionnaire$question_get_choices)
  categorical_data_factored<-sapply(categorical_data,factor) %>% data.frame()
  for(i in 1: ncol(categorical_data_factored)){
    levels_in_data_set<-data.frame(levels_in_dataset=levels(categorical_data_factored[[i]]))
    levels_in_questionnaire<-data.frame(levels_in_questionnaire=categorical_levels[[i]])
    levels_in_questionnaire_properly_ordered<-
      dplyr::full_join(levels_in_data_set, levels_in_questionnaire, by= c("levels_in_dataset"= "levels_in_questionnaire"))
    levels(categorical_data_factored)<-levels_in_questionnaire$levels_in_questionnaire
    
  }
  
  categorical_data_factored

}



