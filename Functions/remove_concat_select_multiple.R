
remove_concat_select_multiple<-function(design, questionnaire){
  which_are_select_multiple<-which(
    sapply(names(design), questionnaire$question_is_select_multiple)
  )
  if(length(which_are_select_multiple)!=0){
    design<-design[,-which_are_select_multiple]
  }
  
  }
    