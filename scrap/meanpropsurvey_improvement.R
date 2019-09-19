meanprop_improved<-function(data,
                 variable_to_analyze,
                 by_strata1,
                 by_strata2,
                 na.action,multiple_combined_dissagregations){
  strata1_formula<-paste0("~",by_strata1)
  strata2_formula<-paste0("~", by_strata2)
  by_variable_step1<-paste0(multiple_combined_dissagregations,collapse="+")
  by_variable_formula<-paste("~",by_variable_step1,collapse="")
  
  print(by_variable_formula)
  
  strata1_aggregation_list<-list()
  strata2_aggregation_list<-list()
  overall_aggregation_list<-list()
  more_complex_aggregation_list<-list()
  
  
  for(i in 1:length(variable_to_analyze)){
    print(paste0((i/length(variable_to_analyze))*100,"%"))
    print(variable_to_analyze[i]); print(i)
    variable_of_interest<-variable_to_analyze[i]
    variable_of_interest_vector<-data$variables[,variable_of_interest]
    variable_of_interest_formula<-paste0("~",variable_of_interest)
    
    if (na.action==TRUE & (length(unique(data$variables[,variable_of_interest])) >1)==TRUE){
      
      if(class(data$variables[,variable_of_interest])=="factor"){
        data2<-data
        data2$variables[,variable_of_interest]<-ifelse(is.na( data2$variables[,variable_of_interest]), "not_applicable", as.character(data2$variables[,variable_of_interest]))
        data2$variables[,variable_of_interest]<-factor(data2$variables[,variable_of_interest])
        data2<-data2}
      if(is.numeric(data$variables[,variable_of_interest])==TRUE){
        data2<-data
        data2$variables[,variable_of_interest]<-ifelse(is.na( data2$variables[,variable_of_interest]), 0, as.character(data2$variables[,variable_of_interest]))
        data2$variables[,variable_of_interest]<-as.numeric(data2$variables[,variable_of_interest])
        data2<-data2
      }
    }
    
    
    if(na.action==FALSE| (length(unique(data$variables[,variable_of_interest])) >1)==FALSE){
      data2<-data
    }
    #####damnit need to split these differently if length(levels)==1 ,0 or more
    #one should be svysciprop other svymean
    
    svyby_table1<-svyby(formula(variable_of_interest_formula),
                        by = formula(strata1_formula),
                        data2, 
                        na.rm=TRUE,
                        na.rm.by = FALSE ,
                        FUN = svymean,
                        keep.var = TRUE) %>% data.frame()
  
    svyby_table2<-svyby(formula(variable_of_interest_formula),
                        by = formula(strata2_formula),
                        data2,
                        na.rm = TRUE,
                        na.rm.by = FALSE ,
                        FUN = svymean,
                        keep.var=TRUE) %>% data.frame()
    
    svyby_table3<-svyby(formula(variable_of_interest_formula),
                        by = formula(by_variable_formula),
                        FUN = svymean,
                        na.rm=TRUE,
                        na.rm.by=FALSE,
                        design = data2,
                        keep.var=TRUE) %>% data.frame()
    
    if(length(levels(data2$variable[,variable_of_interest]))==1){
      print(paste0("level==1, ", variable_of_interest))
      
      svy_prop1 <- svyciprop(formula(variable_of_interest_formula), data2,na.rm=TRUE, na.rm.by = FALSE, keep.var=TRUE )
      # svy_prop1<-svy_prop1[[1]] %>% data.frame()
      colnames(svy_prop1)<-variable_of_interest
      # name_cols<-colnames(svy_prop1)
      zm2<-svy_prop1
    }
    
    if(length(levels(data2$variable[,variable_of_interest]))>1|length(levels(data2$variable[,variable_of_interest]))==0){
      
      svy_prop1 <- svymean(formula(variable_of_interest_formula), data2,na.rm=TRUE, na.rm.by = FALSE ) %>% data.frame()
      # name_cols<- svy_prop1  %>% data.frame() %>% t() %>% colnames()
      overall_aggregation_transposed<- svy_prop1  %>% t() 
      
    }
    
    # overall_aggregation_list[[20]] %>% data.frame() %>% mutate(question=rownames(.)) %>% select(question,everything()) %>% gather(key = asdf,val=asdf,mean:SE)
    # overall_aggregation_list[[20]] %>% data.frame() %>% mutate(question=rownames(.)) %>% select(question,everything()) %>% gather(key=stat,val=value,mean:SE)
    lev <- do.call(paste, c(svyby_table3[multiple_combined_dissagregations], sep="_"))
    # name_cols<-c(variable_of_interest, paste0("se.",variable_of_interest))
    
    svyby_table1_strata_removed<-svyby_table1 %>% select(-by_strata1)
    # colnames(svyby_table1_strata_removed)<-name_cols
    
    svyby_table2_strata_removed<-svyby_table2 %>% select(-by_strata2)
    # colnames(svyby_table2_strata_removed)<-name_cols
    
    svyby_table3_strata_removed<- svyby_table3 %>% select(-multiple_combined_dissagregations)
    
    # colnames(svyby_table3_strata_removed)<-name_cols
    
    # colnames(svy_prop1)<-name_cols
    
    strata1_aggregation_list[[i]]<-svyby_table1_strata_removed
    strata2_aggregation_list[[i]]<- svyby_table2_strata_removed
    overall_aggregation_list[[i]] <- svy_prop1 %>% t()
    
    
    
    more_complex_aggregation_list[[i]]<-svyby_table3_strata_removed %>% data.frame()}
    
    
    cldf <- do.call("cbind", strata1_aggregation_list) %>% data.frame()
    cldf <- cldf %>% mutate(Level = "Camp", Level_id = svyby_table1[,by_strata1]) %>%
      select(Level, Level_id, everything())
    print("camp level complete")
    
    uldf <-do.call("cbind", strata2_aggregation_list) %>% data.frame
    uldf <-uldf %>% mutate(Level = "Upazilla", Level_id = svyby_table2[,by_strata2]) %>%
      select(Level, Level_id, everything())
    print("Upazila level complete")
    
    rldf <-do.call("cbind", overall_aggregation_list) %>% data.frame
    rldf <-rldf %>% mutate(Level = "Response", Level_id ="Response") %>%
      select(Level, Level_id, everything())
    print("Response level complete")
    
    gldf <-do.call("cbind", more_complex_aggregation_list) %>% data.frame
    gldf <-gldf %>% mutate(Level = "Response_Gender", Level_id = lev) %>%
      select(Level, Level_id, everything())
    print("Response level cross gender complete")
    
    all_strata <- do.call("smartbind", list(cldf, uldf, rldf,gldf))
    
    return(list(all_strata,cldf, uldf, rldf,gldf))
  }
  