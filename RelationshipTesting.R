#relationship testing

rm(list=ls())
user<-"zack"
population<-c("Host","Refugee")[1]
data_process<-c("checking", "cleaning", "analysis")[3]
analysis_phase<-c("basic","relationship_testing")[2]

library(dplyr)
# library(GISutils)
# detach("package:butteR", unload = TRUE)
library(nngeo)
library(dplyr)
library(hypegrammaR)
library(koboquest)
library(stringr)
library(lubridate)
library(rgdal)
library(sf)
library(anytime)
library(srvyr)
library(forcats)

source("Functions/colours.R")
source("Functions/ActivatePaths.R")
source("Functions/make_composite_indicators_bgd_msna_2019.R")
source("Functions/calculate_shelter_topology.R")
source("Functions/general_utils.R")
source("Functions/recode_to_severity_bgd_msna2019.R")
source("Functions/recoding_severity/recode_SNFI_severity_bgd2019.R")
source("Functions/recoding_severity/recode_HEALTH_severity_bgd2019.R")
source("Functions/recoding_severity/recode_COPING_severity_bgd2019.R")
source("Functions/recoding_severity/recode_EDUCATION_severity_bgd2019.R")
source("Functions/recoding_severity/recode_WASH_severity_bgd2019.R")



# LOAD DATA ---------------------------------------------------------------

HH_kobo_questions<-read.csv(survey_path,stringsAsFactors = FALSE)
HH_kobo_choices<-read.csv(choices_path, stringsAsFactors = FALSE)
wash_combo_table<-read.csv(wash_severity_combination_path,, stringsAsFactors = FALSE, na.strings=c("", " ", NA, "NA"))

HH<-read.csv(HH_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA, "NA"))
Indiv<-read.csv(Indiv_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA, "NA"))
HH_kobo_questionnaire<-koboquest::load_questionnaire(HH,questions = HH_kobo_questions,choices = HH_kobo_choices, choices.label.column.to.use = "label..english")
pop<- read.csv(pop_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA))

HH_yes_consent<- HH %>% filter(informed_consent=="yes")
indiv_yes_consent<-Indiv %>% filter(X_submission__uuid %in% HH_yes_consent$X_uuid)
# debugonce(make_composite_indicators_bgd_msna_2019)

composite_indicators<-make_composite_indicators_bgd_msna_2019(hh_data = HH_yes_consent,  individual_data = indiv_yes_consent,population = population)



#RUN COMPOSITE INDICATOR SCRIPT- THANKS MEHEDI!
composite_indicators<-make_composite_indicators_bgd_msna_2019(hh_data = HH_yes_consent,  individual_data = Indiv,population = population)

# JOIN COMPOSITE INDICATORS TO APPROPRIATE DATA SETS
HH_with_composite<-HH_yes_consent %>% left_join(composite_indicators$household_composites,by="X_uuid")
HH_with_composite<-HH_with_composite %>% 
  mutate(I.coping.new_debt=hh_coping_mechanism.borrowed| hh_coping_mechanism.bought_on_credit)


Indiv_with_composite<-Indiv %>% left_join(composite_indicators$individual_composites,by=c("X_submission__uuid"))

HH_data_factorized <- butteR::questionnaire_factorize_categorical(data=HH_with_composite,questionnaire = HH_kobo_questionnaire,return_full_data = TRUE)
# HH_data_factorized<-HH_with_composite

HH_data_factorized<-HH_data_factorized %>%  #REMOVE COLUMNS THAT ARE ALL NAS
  select_if(~!all(is.na(.)))

# HH_data_factorized<- butteR::questionnaire_factorize_categorical(HH_data_factorized,HH_kobo_questionnaire)

# CLEAN POPULATION DATA ---------------------------------------------------

if (population=="Refugee") {
  
  pop_cleaned<-pop %>% 
    filter(!is.na(Camp)& is.na(Block)) %>% 
    mutate(
      !!(sf_strata):=str_replace(Camp, "Total","") %>% trimws(),
      Total.Families=as.numeric(Total.Families)
    ) %>% 
    select({sf_strata}, Total.Families,Total.Individuals)}
if(population=="Host"){
  pop_cleaned<-pop
  pop_cleaned[[sf_strata]]<-if_else(pop_cleaned[[sf_strata]] =="Teknaf Sadar" , "Teknaf",pop_cleaned[[sf_strata]])
}

weighting<-map_to_weighting(sampling.frame = pop_cleaned, 
                            data.stratum.column = strata,
                            data = HH_data_factorized, 
                            sampling.frame.population.column =sf_pop,
                            sampling.frame.stratum.column = sf_strata)




#MAKE DESIGN OBJECTS

HH_svy_ob<-map_to_design(data=HH_data_factorized, weighting_function = weighting)

HH$euc_dist_to_camp_m
Indiv_with_relevant_HH_data<-Indiv_with_composite %>% left_join(HH_svy_ob$variables %>% 
                                                                  mutate(
                                                                    weights= weights(HH_svy_ob)
                                                                    ) %>% 
                                                                  select(X_uuid,{strata},respondent_gender,weights,euc_dist_to_camp_m), by=c("X_submission__uuid"="X_uuid"))

ind_svy_ob<-survey::svydesign(ids = ~ 1,
                              strata =  formula(paste0("~",strata)),
                              weights= ~weights,
                              data = Indiv_with_relevant_HH_data)




independent_variables<- c("I.HH_CHAR.dependency_ratio_classification.HH", "I.HH_CHAR.size.HH", "I.HH_CHAR.gender_hoh.HH", "I.HH_CHAR.education_level.HH", "I.HEALTH.indhelp_atleast.INDVHH")


dependent_variables<-c("I.FSL.livelihoods_atleast_one.INDVHH", "I.coping.new_debt") %>% trimws()
svyby(~I.FSL.livelihoods_atleast_one.INDVHH, ~I.HH_CHAR.dependency_ratio_classification.HH, svymean, HH_svy_ob )

)

HH_srvy_ob$variable$I.FSL.livelihoods_atleast_one.INDVHH
HH_srvy_ob<-srvyr::as_survey(HH_svy_ob)
HH_srvy_ob$variables$I.FSL.livelihoods_atleast_one.INDVHH %>% class(
  
  
)

HH_srvy_ob %>%
  group_by(I.HH_CHAR.dependency_ratio_classification.HH,I.FSL.livelihoods_atleast_one.INDVHH,.drop=FALSE) %>%
  summarise(mean.stat=survey_mean(na.rm=TRUE,vartype="ci" )) 


HH_srvy_ob %>% 
  group_by( I.FSL.livelihoods_atleast_one.INDVHH) %>% 
  summarise(survey_mean(na.rm=TRUE,vartype="ci"))


asdf<-survey::svyby(~I.FSL.livelihoods_atleast_one.INDVHH, ~I.HH_CHAR.dependency_ratio_classification.HH,HH_svy_ob, svymean,vartype="ci", na.rm=T) %>% data.frame() 
asdf<-survey::svychisq(~I.HH_CHAR.dependency_ratio_classification.HH+I.FSL.livelihoods_atleast_one.INDVHH,HH_svy_ob)
asdf$p.value

asdf<-survey:: (~I.FSL.livelihoods_atleast_one.INDVHH, ~I.HH_CHAR.dependency_ratio_classification.HH,HH_svy_ob, svymean,vartype="ci", na.rm=T) %>% data.frame() 


asdf

plot_list<-list()
svyby_table_list<-list()
significant_table_list<-list()
for ( i in 1:length(dependent_variables)){
  print(dependent_variables[i])
  independent_string<-"I.HH_CHAR.dependency_ratio_classification.HH"
  dependent_string<-dependent_variables[i]
  
  dependent_string.yesans<-paste0(dependent_string,"yes")
  se_dependent_string<-paste0("se.", dependent_string)
  formula_dependent<-paste0("~", dependent_string)
  formula_independent<-paste0("~",independent_string)
  table_formula<-(paste0(formula_independent, "+", dependent_string))
  print(table_formula)
  result_chisq<-svychisq(formula(table_formula),HH_svy_ob)
  print("chisq stat calculated")
  pval<-result_chisq$p.value
  pval_label<-ifelse(pval<0.05,"<.05", ifelse(pval<.005,"<.005",">.05"))
  print("pval label made")
  # plot_title<-paste0(plot_titles[i]," (p",pval_label,")")
  plot_title<-paste0(dependent_string," (p",pval_label,")")
  svyby_table<-svyby(formula(formula_dependent), formula(formula_independent),HH_svy_ob, svymean, na.rm=T) %>% data.frame() 
  print("svytable made")
  
  
  dependent_string.yes<-svyby_table %>%
    select(intersect(starts_with(dependent_string),
                     ends_with("yes"))) %>% colnames()
  print("asdf")
  se_dependent_string.yes<-svyby_table %>% 
    select(intersect(starts_with(se_dependent_string), 
                     ends_with("yes"))) %>% colnames()
  
  svyby_table<-svyby_table %>% 
    mutate(
      err_ymin=!!sym(dependent_string.yes)-(1.96*!!sym(se_dependent_string.yes)),
      err_ymax=!!sym(dependent_string.yes)+(1.96*!!sym(se_dependent_string.yes)),
    )
  print("asdfgh")
  er<-geom_errorbar(data = svyby_table,aes(ymin=err_ymin,
                                           ymax=err_ymax), color="white", size=2,width=0.2)
  plot_list[[dependent_string]]<-
    ggplot(svyby_table,aes(factor(!!sym(independent_string)), !!sym(dependent_string.yes)))+
    geom_bar(stat= "identity", colour="white")+
    er+
    xlab("I.indi_dia_per_INDIVHH")+ 
    ggtitle(plot_title)+
    scale_y_continuous(labels = scales::percent)+
    theme(
      panel.grid = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(angle=45,size = 10),
      # text = element_text(size=10)
    )
  
  svyby_table_list[[dependent_string]]<-svyby_table
  
  if(result_chisq$p.value<.05){
    significant_table_list[[dependent_string]]<-svyby_table
  }
  if(result_chisq$p.value>=0.05){
    not_significant_table_list[[dependent_string]]<-svyby_table
  }
}






m1<-svyglm(formula = cooking_fuel.collected_firewood~ euc_dist_to_camp_m,
                     HH_svy_ob)
sjPlot::tab_model(m1, auto.label = FALSE,title = "Table 1. dist camp")# proxmity to camp


HH_svy_ob$variables$tension<-HH_svy_ob$variables$tension %>% as.factor()
HH_svy_ob$variables$tension_binary<-ifelse(HH_svy_ob$variables$tension=="yes",1,0)
m2<-svyglm(formula = tension_binary~ euc_dist_to_camp_m,HH_svy_ob)

plot(allEffects(m2))
sjPlot::tab_model(m2, auto.label = FALSE,title = "Table 1. dist camp")# proxmity to camp


HH_svy_ob$variables$humanitarian_aid
HH_svy_ob$variables$humanitarian_aid_yes_binary<-if_else(HH_svy_ob$variables$humanitarian_aid=="yes",1,0)

m3<-svyglm(formula = humanitarian_aid_yes_binary~ euc_dist_to_camp_m,HH_svy_ob)
plot(allEffects(m3))
sjPlot::tab_model(m3, auto.label = FALSE,title = "Table 1. dist camp")# proxmity to camp


plot(HH_svy_ob$variables$I.FCS_score,HH_svy_ob$variables$euc_dist_to_camp_m)


m4<-svyglm(formula = humanitarian_aid_yes_binary~ euc_dist_to_camp_m,HH_svy_ob)
plot(allEffects(m3))
sjPlot::tab_model(m3, auto.label = FALSE,title = "Table 1. dist camp")# proxmity to camp


HH_svy_ob$variables$I.WASH.enough_water_all_needs.HH<-if_else(HH_svy_ob$variables$I.WASH.enough_water_all_needs.HH=="yes",1,0)
m5<-svyglm(formula = I.WASH.enough_water_all_needs.HH~ euc_dist_to_camp_m,HH_svy_ob)
plot(allEffects(m5))
sjPlot::tab_model(m5, auto.label = FALSE,title = "Table 1. dist camp")# proxmity to camp


HH_svy_ob$variables$surface_Water_dry<-if_else(HH_svy_ob$variables$surface_water_access %in% c("dont_know", "never"),0,1)
m6<-svyglm(formula = surface_Water_dry~ euc_dist_to_camp_m,HH_svy_ob)
plot(allEffects(m6))
sjPlot::tab_model(m6, auto.label = FALSE,title = "Table 1. dist camp")# proxmity to camp
m6$coefficients[2] %>% exp
exp( coef(m6) , confint(m6))
exp(coefficients(m6), confint(m6))


HH_svy_ob$variables$I.FLS.at_leastoneworking_binary = if_else(HH_svy_ob$variables$I.FSL.livelihoods_atleast_one.INDVHH=="yes",1,0,0)

HH_svy_ob$variables$I.HH_CHAR.dependency_ratio.INDVHH



m7<-svyglm(formula = I.FLS.at_leastoneworking_binary~ I.HH_CHAR.dependency_ratio.INDVHH,HH_svy_ob)

m7<-svyglm(formula = I.FLS.at_leastoneworking_binary~ I.HH_CHAR.dependency_ratio.INDVHH,HH_svy_ob, na.action=TRUE)
?svyglm

# dist to camp by camp ----------------------------------------------------

p8 <- ggplot(HH_data_factorized, aes(x = euc_dist_to_camp_m, fill = factor(!!sym(strata)))) +
  geom_density(position="identity", alpha=0.6) +
  # scale_x_continuous(name = "Mean ozone in\nparts per billion",
                     # breaks = seq(0, 200, 25),
                     # limits=c(0, 200)) +
  scale_y_continuous(name = "Density") +

  ggtitle("Dist to Camp") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma")) #+
  # scale_fill_brewer(palette="Accent")

p8

