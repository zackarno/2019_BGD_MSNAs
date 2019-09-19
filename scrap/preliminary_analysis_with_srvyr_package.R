
library(dplyr)
library(GISutils)
library(srvyr)
library(sf)
library(raster)


# Make grid ---------------------------------------------------------------

hh_data<-HH_svy_full$variables
hh_sf<-st_as_sf(HH, coords= long_lat, crs=sf::st_crs(strata_spatial))
hh_sf_utm<- sf::st_transform(hh_sf, 32646)

grid_utm<-hh_sf_utm %>% 
  st_bbox %>% 
  st_as_sfc %>% 
  st_make_grid(cellsize=100) %>% 
  st_sf
asdf<-st_join(hh_sf_utm, grid_utm)


grid_utm<-sf::st_make_grid(x = st_as_sfc(st_bbox(hh_sf_utm)),cellsize=100)
jj<-st_join(hh_sf_utm,grid_utm)
st_crs(hh_sf_utm)
class(hh_sf_utm)
st_crs(grid_utm)
class(grid_utm)
class(strata_spatial)

grid_utm<-sf::st_make_grid(x = hh_sf_utm,cellsize=100)
sf::st_sfc()
grid_utm<-grid_utm %>% 
  mutate(id=1:n())

sf::as
jj<-st_join(hh_sf_utm,sf::st_as_sf(grid_utm))

grid_utm
plot(grid_utm); plot(hh_sf_utm, col="red")
nz_avheight = aggregate(x = nz_height, by = nz, FUN = mean)
H_spatial<-sf::st_join(HH_spatial_utm,strata_spatial_utm)
H_spatial<-sf::st_join(H_spatial,grid_utm)

asdf<-sf::st_join(hh_sf_utm ,grid_utm)
H_spatial<-sf::st_join(HH_spatial_utm,strata_spatial_utm)
as.SpatialPoints(hh_sf_utm)
# use kde.points to create a kernel density surface
breach.dens = st_as_sf(kde.points(breach,lims=tracts))
summary(breach.dens)
?kde.points

asdf<-hh_sf_utm  %>%  sf::st_interpolate_aw(grid_utm, extensive=TRUE)
asdf<-st_cast(hh_sf_utm, grid_utm)
hh_sp_pts <- as(hh_sf_utm, "Spatial")
kde_hh<-GISTools::kde.points(pts = hh_sp_pts,n = 100)
?GISTools::kde.points
kde_hh
kde_hh %>% class()
plot(kde_hh)
bla<-hh_sf_utm%>% 
  st_join(grid_utm,join = "intersect")

tree_in_tract <- st_join(hh_sf_utm,grid_utm, join = st_within)
class(grid_utm  )
?sf::st_make_grid
bla<-grid_utm %>% 
  sf::st_join(hh_sf_utm,join = st_intersect) 

sf::st_join()
ggplot()+
  geom_sf(data=grid_utm)+
  geom_sf(data=hh_sf_utm,aes(color="camp_id")) 


HH_svy_full$variables %>%  select_if(is.integer) %>% colnames() %>% dput()

variable_names<-c("shelter_purchased_reason", "shelter_paid", 
                  "parent_committee","water_source_drink.unprotected_dugwell", "water_source_drink.unprotected_spring", 
                  "water_source_drink.surface_water", "water_source_drink.dont_know", 
                  "water_source_drink.other", "water_source_wash.pipe_water", "water_source_wash.tubewells", 
                  "water_source_wash.protected_dugwell", "water_source_wash.protected_spring")

HH_svy_full$variables %>% 
  group_by(NULL)
f1(HH_svy_full$variables,"respondent_hoh", asdf = "camp_id", "hoh_gender")


variable_names<-colnames(HH_svy_full$variables)[200:300]

check_analysis_nas_removed<-aggregate_analyze_survey(design = HH_svy_full,
                                                     list_of_variables = variable_names,
                                                     aggregation_level = "",
                                                     round_to =2, 
                                                     return_confidence = FALSE,
                                                     na_replace=TRUE)


test_butter<-butteR::mean_proportion_table(design = HH_svy_full,
                              list_of_variables = variable_names,
                              aggregation_level = NULL,
                              round_to =2, 
                              return_confidence = FALSE,
                              na_replace=TRUE)

debugonce(aggregate_analyze_survey)
testing1<-aggregate_analyze_survey(design = HH_svy_full,
                                   list_of_variables = variable_names,
                                   aggregation_level = NULL,
                                   round_to = 3, 
                                   return_confidence = FALSE,
                                   na_replace=TRUE)
testing2<-aggregate_analyze_survey(design = HH_svy_full,
                                   list_of_variables = variable_names,
                                   aggregation_level = NULL,
                                   round_to = 3, 
                                   return_confidence = FALSE,
                                   na_replace=FALSE)


# as_survey(HH_svy_full) %>% 
#   group_by(parent_committee,.drop=FALSE) %>% 
#   summarise(mean.stat=survey_mean(na.rm=FALSE,vartype="ci", ))

check_analysis_nas_replaced==check_analysis_nas_removed

data.frame(check_analysis_nas_removed$camp_id, na_replaced=check_analysis_nas_replaced$water_source_wash.protected_dugwell,na_removed=check_analysis_nas_removed$water_source_wash.protected_dugwell)

###############################
#butteR function #1
#################################################################
aggregate_analyze_survey<-function(design, 
                                   list_of_variables,
                                   aggregation_level,
                                   round_to=2,
                                   return_confidence=TRUE,
                                   na_replace=FALSE){
  design_srvy<-as_survey(design)
  integer_analysis_tables<-list()
  factor_analysis_tables<-list()
  for(i in 1: length(list_of_variables)){
    variable_to_analyze<-list_of_variables[i]
    print(variable_to_analyze)
    
    if(class(design_srvy$variables[[variable_to_analyze]])=="integer"){
      if(na_replace==TRUE){
        design_srvy$variables[[variable_to_analyze]]<-ifelse(is.na(design_srvy$variables[[variable_to_analyze]]),
                                                             0,design_srvy$variables[[variable_to_analyze]])
      }
      if(return_confidence==TRUE)
      {
        integer_analysis_tables[[i]]<-design_srvy %>% 
          group_by(!!sym(aggregation_level),.drop=FALSE) %>% 
          summarise(mean.stat=survey_mean(!!sym(variable_to_analyze),na.rm=TRUE,vartype="ci")) %>% 
          mutate(!!variable_to_analyze:=paste0(round(mean.stat,round_to),
                                               " (",round(mean.stat_low,round_to),
                                               ", ", round(mean.stat_upp,round_to),
                                               ")")) %>% 
          dplyr::select(aggregation_level, variable_to_analyze)
      }
      
      if(return_confidence==FALSE)
      {
        integer_analysis_tables[[i]]<-design_srvy %>% 
          group_by(!!sym(aggregation_level),.drop=FALSE) %>% 
          summarise(!!variable_to_analyze:=survey_mean(!!sym(variable_to_analyze),na.rm=TRUE,vartype="ci")) %>% 
          select(!!sym(aggregation_level), !!sym(variable_to_analyze))
      }
    }
    
    if(class(design_srvy$variables[[variable_to_analyze]])=="factor"){
      if(na_replace==TRUE){
        design_srvy$variables[[variable_to_analyze]]<-forcats::fct_explicit_na(HH_svy_full$variables[,variable_to_analyze], "filtered_values")
      }
      
      if(return_confidence==TRUE){
        factor_analysis_tables[[i]]<-design_srvy %>% 
          group_by(!!sym(aggregation_level),!!sym(variable_to_analyze),.drop=FALSE) %>% 
          summarise(mean.stat=survey_mean(na.rm=TRUE,vartype="ci" )) %>% 
          gather("question","answer_choice",2) %>% 
          mutate(question.response=paste0(question,".", answer_choice)) %>% 
          select(!!sym(aggregation_level), question.response, mean.stat:mean.stat_upp)
      }
      if(return_confidence==FALSE){
        factor_analysis_tables[[i]]<-design_srvy %>% 
          group_by(!!sym(aggregation_level),!!sym(variable_to_analyze),.drop=FALSE) %>% 
          summarise(mean.stat=survey_mean(na.rm=TRUE,vartype="ci" )) %>% 
          gather("question","answer_choice",2) %>% 
          mutate(question.response=paste0(question,".", answer_choice))
      }
    }}
  print("binding intergers")
  
  interger_analysis_tables_full<- integer_analysis_tables[-which(sapply(integer_analysis_tables, is.null))]
  integers_analyzed_wide<-Reduce(function(x, y) merge(x, y, by =aggregation_level, all = TRUE), interger_analysis_tables_full)
  integers_analyzed_wide<-integers_analyzed_wide %>% 
    select_if(~!all(is.na(.))) %>% select(-ends_with(".NA"))
  print("binding factors")
  factors_analyzed_long<-do.call("rbind", factor_analysis_tables)
  print("spreading factors")
  if(return_confidence==TRUE){
    factors_analyzed_wide<-factors_analyzed_long %>% 
      mutate(stat_full= paste0(round(mean.stat,round_to),
                               " (",round(mean.stat_low,round_to),
                               ", ", round(mean.stat_upp,round_to),
                               ")")) %>% 
      select(!!sym(aggregation_level), question.response, stat_full) %>% tidyr::spread(question.response,stat_full) %>% 
      select_if(~!all(is.na(.))) %>% 
      select(-ends_with(".NA"))}
  
  if(return_confidence==FALSE){
    factors_analyzed_wide<-factors_analyzed_long %>% 
      select(!!sym(aggregation_level), question.response, mean.stat) %>% tidyr::spread(question.response,mean.stat) %>% 
      select_if(~!all(is.na(.)))
    
  }
  
  
  combined_output<-left_join(factors_analyzed_wide, integers_analyzed_wide, by=aggregation_level)
}

######################################################
#butteR function # 2
asdf<-NULL

######################################################
aggregate_analyze_survey<-function(design, 
                                   list_of_variables,
                                   aggregation_level,
                                   round_to=2,
                                   return_confidence=TRUE,
                                   na_replace=FALSE){
  design_srvy<-as_survey(design)
  integer_analysis_tables<-list()
  factor_analysis_tables<-list()
  
  for(i in 1: length(list_of_variables)){
    variable_to_analyze<-list_of_variables[i]
    
    print(variable_to_analyze)
    
    if(class(design_srvy$variables[[variable_to_analyze]])=="integer"){
      if(na_replace==TRUE){
        design_srvy$variables[[variable_to_analyze]]<-ifelse(is.na(design_srvy$variables[[variable_to_analyze]]),
                                                             0,design_srvy$variables[[variable_to_analyze]])
      }
      aggregate_by<- syms(aggregation_level)
      if(is.null(aggregation_level)) {
        integers_formatted_for_analysis<-design_srvy 
      }
      else {
        integers_formatted_for_analysis<-design_srvy %>% 
          group_by(!!!aggregate_by,.drop=FALSE)
      }
      
      if(return_confidence==TRUE)
      {
        integer_analysis_tables[[i]]<-integers_formatted_for_analysis %>% 
          summarise(mean.stat=survey_mean(!!sym(variable_to_analyze),na.rm=TRUE,vartype="ci")) %>% 
          mutate(!!variable_to_analyze:=paste0(round(mean.stat,round_to),
                                               " (",round(mean.stat_low,round_to),
                                               ", ", round(mean.stat_upp,round_to),
                                               ")")) %>% 
          dplyr::select(!!!aggregate_by, variable_to_analyze)
      }
      
      if(return_confidence==FALSE)
      {
        integer_analysis_tables[[i]]<-integers_formatted_for_analysis %>% 
          summarise(!!variable_to_analyze:=survey_mean(!!sym(variable_to_analyze),na.rm=TRUE,vartype="ci")) %>% 
          select(!!!aggregate_by, !!sym(variable_to_analyze))
      }
    }
    
    if(class(design_srvy$variables[[variable_to_analyze]])=="factor"){
      if(na_replace==TRUE){
        design_srvy$variables[[variable_to_analyze]]<-forcats::fct_explicit_na(HH_svy_full$variables[,variable_to_analyze], "filtered_values")
      }
      if(is.null(aggregation_level)){
        aggregate_by<-syms(variable_to_analyze)
        factors_analyzed<-design_srvy %>% 
          group_by(!!!aggregate_by,.drop=FALSE) %>% 
          summarise(mean.stat=survey_mean(na.rm=TRUE,vartype="ci" )) %>% 
          gather("question","answer_choice",variable_to_analyze) %>% 
          mutate(question.response=paste0(question,".", answer_choice)) %>% 
          select(question.response,mean.stat:mean.stat_upp)
      } else { 
        aggregate_by<-syms(c(aggregation_level,variable_to_analyze))
        factors_analyzed<-design_srvy %>% 
          group_by(!!!aggregate_by,.drop=FALSE) %>% 
          summarise(mean.stat=survey_mean(na.rm=TRUE,vartype="ci" )) %>% 
          gather("question","answer_choice",variable_to_analyze) %>% 
          mutate(question.response=paste0(question,".", answer_choice)) %>% 
          select(!!(aggregation_level), question.response, mean.stat:mean.stat_upp)
      }
      if(return_confidence==TRUE){
        factor_analysis_tables[[i]]<-factors_analyzed
      }
      if(return_confidence==FALSE){
        factor_analysis_tables[[i]]<-design_srvy %>% 
          group_by(!!!aggregate_by,!!sym(variable_to_analyze),.drop=FALSE) %>% 
          summarise(mean.stat=survey_mean(na.rm=TRUE,vartype="ci")) %>% 
          gather("question","answer_choice",variable_to_analyze) %>% 
          mutate(question.response=paste0(question,".", answer_choice)) 
      }
    }}

  
  print("binding intergers")
  interger_analysis_tables_full<- integer_analysis_tables[-which(sapply(integer_analysis_tables, is.null))]
  integers_analyzed_wide<-Reduce(function(x, y) merge(x, y, by =aggregation_level, all = TRUE), interger_analysis_tables_full)
  integers_analyzed_wide<-integers_analyzed_wide %>% 
    select_if(~!all(is.na(.))) %>% select(-ends_with(".NA"))
  print("binding factors")
  factors_analyzed_long<-do.call("rbind", factor_analysis_tables)
  print("spreading factors")
  if(return_confidence==TRUE){
    if(is.null(aggregation_level)){
      factors_analyzed_wide<-factors_analyzed_long %>%
        mutate(stat_full= paste0(round(mean.stat,round_to),
                                 " (",round(mean.stat_low,round_to),
                                 ", ", round(mean.stat_upp,round_to),
                                 ")")) %>%
        select(question.response, stat_full) %>% tidyr::spread(question.response,stat_full) %>%
        select_if(~!all(is.na(.))) %>%
        select(-ends_with(".NA"))
    } else {
      factors_analyzed_wide<-factors_analyzed_long %>% 
        mutate(stat_full= paste0(round(mean.stat,round_to),
                                 " (",round(mean.stat_low,round_to),
                                 ", ", round(mean.stat_upp,round_to),
                                 ")")) %>% 
        select(!!(aggregation_level), question.response, stat_full) %>% tidyr::spread(question.response,stat_full) %>% 
        select_if(~!all(is.na(.))) %>% 
        select(-ends_with(".NA")) %>% 
        filter(!is.na(!!sym(aggregation_level[length(aggregation_level)])))
    }}
  
    if(return_confidence==FALSE){
      if(is.null(aggregation_level)) {
        factors_analyzed_wide<-factors_analyzed_long %>% 
          select(question.response, mean.stat) %>% tidyr::spread(question.response,mean.stat) %>% 
          select_if(~!all(is.na(.)))%>% 
          select(-ends_with(".NA")) 
      } else {
        factors_analyzed_wide<-factors_analyzed_long %>% 
          select(aggregation_level, question.response, mean.stat) %>% tidyr::spread(question.response,mean.stat) %>% 
          select_if(~!all(is.na(.)))%>% 
          select(-ends_with(".NA")) %>% 
          filter(!is.na(!!sym(aggregation_level[length(aggregation_level)])))
      }
    }
  if (is.null(aggregation_level)) {
    combined_output<-cbind(factors_analyzed_wide, integers_analyzed_wide)
    
  } else{
    combined_output<-left_join(factors_analyzed_wide, integers_analyzed_wide, by=aggregation_level)}
}
  ############################################################################################################
  
  aggregate_analyze_survey(design=HH_svy_full$ ,list_of_variables = c("respondent_gender",))
  HH_srvy<-as_survey(HH_svy_full)
  
  srvyr_factors<-srv.ex$variables %>%  select_if(is.factor) %>% colnames() 
  srvyr_integers<-srv.ex$variables%>% select_if(is.integer) %>% colnames()
  
  
  
  integer.test<-srvyr_integers
  integer_list<-list()
  for(i in 1:length(integer.test)){
    
    voi<-integer.test[i]
    integer_list[[i]]<-HH_srvy %>% 
      group_by(camp_id) %>% 
      summarise(!!voi:=survey_mean(!!sym(voi),na.rm=TRUE,vartype="ci"), )
  }
  integer_list
  integer_df<-Reduce(function(x, y) merge(x, y, by ="camp_id", all = TRUE), integer_list)
  
  factor.test<-srvyr_factors[10:30]
  factor_list<-list()
  for(i in 1:length(factor.test)){
    voi<-factor.test[i]
    voi.name=paste0(voi,".stat")
    factor_list[[i]]<-srv.ex %>% 
      group_by(camp_id,!!sym(voi),.drop=FALSE) %>% 
      summarise(mean.stat=survey_mean(na.rm=TRUE,vartype="ci" )) %>% 
      gather("question","answer_choice",2) %>% 
      mutate(question.response=paste0(question,".", answer_choice)) %>% 
      select(camp_id, question.response, mean.stat:mean.stat_upp)
    
  }
  
  factors_analyzed_long<-do.call("rbind", factor_list)
  factors_analyzed_wide<-factors_analyzed_long %>% 
    mutate(stat_full= paste0(round(mean.stat,2),
                             " (",round(mean.stat_low,2),
                             ", ", round(mean.stat_upp,2),
                             ")")) %>% 
    select(camp_id, question.response, stat_full) %>% tidyr::spread(question.response,stat_full)
  
  dim(factors_analyzed_wide)
  dim(integer_df)  
  analysis_wide<-left_join(factors_analyzed_wide,integer_df, by="camp_id")
  
  
  grouper<-paste0("!!sym(",asdf,")",collapse = ",") 
  
  length(grouper)
  !!sym("asdf")
  collapse
  integer.test<-srvyr_integers[10:30]
  integer_list<-list()
  for(i in 1:length(integer.test)){
    grouper<-paste0("!!sym(",asdf,")",collapse = ",")
    grouper<-quos(grouper)
    voi<-integer.test[i]
    print(voi)
    integer_list[[i]]<-HH_srvy %>% 
      group_by(!!!grouper) %>% 
      summarise(!!voi:=survey_mean(!!sym(voi),na.rm=TRUE,vartype="ci"), )
  }
  
  asdf<-c("camp_id","hoh_gender")
  HH_srvy$variables$hoh_gender
  