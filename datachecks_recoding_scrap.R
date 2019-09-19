
HH$health_coping




HH %>% 
  mutate(
    debt_health_contradiction=ifelse(health_coping.ghealth_debt== 1&debt_reason.pay_health_expenses== 0, "yes", "no")
  ) %>% 
  filter(debt_health_contradiction=="yes") %>% 
  select(X_uuid,enumerator_id,health_coping.ghealth_debt, debt_reason.pay_health_expenses,debt_health_contradiction)
  
Indiv$ind_why_notreatment %>% unique()
Indiv$ind_why_notreatment.safety_concerns
Indiv$ind_why_notreatment.safety_concerns_night



Indiv %>% 
  mutate(
    no_treatment_safety_concerns=ifelse(ind_why_notreatment.safety_concerns==1|ind_why_notreatment.safety_concerns_night==1,1,0),
    ) %>% 
  group_by(X_submission__uuid) %>% 
  summarise(any_safety_concerns= ifelse(sum(no_treatment_safety_concerns,na.rm=TRUE)>0,1,0)) %>% 
  left_join(HH %>% 
              select(X_uuid,
                     feel_unsafe_female.health_center,
                     feel_unsafe_male.health_center,
                     feel_unsafe_female.way_to_facilities,
                     feel_unsafe_male.way_to_facilities), by=c("X_submission__uuid"="X_uuid")) %>% 
  mutate(
    any_feel_unsafe.health_center=ifelse(feel_unsafe_female.health_center==1| feel_unsafe_male.health_center==1,1,0),
    any_feel_unsafe_way_to_facilities=ifelse(feel_unsafe_female.way_to_facilities==1| feel_unsafe_female.way_to_facilities==1,1,0)
  ) %>% 
  filter(any_safety_concerns==1)


####################
#
HH$debt_reason.pay_house_rent
HH$shelter_paid %>% unique()
HH$exp_rent %>% unique()


#RENT CONTRADICTION- DONE
###############################################################3
if(population=="Refugee"){ 
  rent_paid_q<-"shelter_paid"
}
if(population=="Host"){
  rent_paid_q<-"rent_host"  
}

HH %>% 
  mutate(
    shelter_rent_contra= ifelse(exp_rent %in% c(NA,"0_bdt")& HH[[rent_paid_q]]=="yes", "yes", "no")
    ) %>% 
  select(X_uuid,enumerator_id, shelter_rent_contra) %>% 
  filter(shelter_rent_contra=="yes") %>% 
  arrange(enumerator_id)
#######################################################################3

HH$improvement %>% unique()
HH$improvement.none
HH$exp_shelter_materials %>% unique()

#SHELTER EXPENDITURE CONTRADICTIONS - REFUGEE HOST COMMUNITY
##############################################################
HH %>% 
  mutate(
    shelter_expenditures_contradiction=ifelse((exp_shelter_materials %in% c(NA,"0_bdt")==FALSE) & improvement.none==1, "yes", "no")
  ) %>% 
  select(X_uuid,enumerator_id, exp_shelter_materials, improvement.none,shelter_expenditures_contradiction) %>% 
  filter(shelter_expenditures_contradiction=="yes") 
##############################################################

#DEBT CONTRADICTION
#######################################################################################################
debt_reason_columns<-HH %>% select(starts_with("debt_reason.")) %>% colnames()
HH %>% 
  mutate(
    any_debt_reason= ifelse(rowSums(.[debt_reason_columns],na.rm=TRUE)>0,1,0),
    debt_expense_contradiction= ifelse(any_debt_reason==1 & exp_debt%in% c(NA,"0_bdt"),"yes","no")
  ) %>% 
  select(X_uuid,enumerator_id, any_debt_reason,exp_debt,debt_expense_contradiction) %>%
  filter(debt_expense_contradiction=="yes") %>% 
  arrange(enumerator_id)
#######################################################################################################


Indiv %>% 
  mutate(
    no_treatment_safety_concerns=ifelse(ind_why_notreatment.safety_concerns==1|ind_why_notreatment.safety_concerns_night==1,1,0),
  ) %>% select(no_treatment_safety_concerns,ind_why_notreatment.safety_concerns_night, ind_why_notreatment.safety_concerns) %>% 
  filter(ind_why_notreatment.safety_concerns_night==1)
  
group_by(X_submission__uuid) %>% 
  summarise(any_safety_concerns= ifelse(sum(no_treatment_safety_concerns,na.rm=TRUE)>0,1,0)) %>% 
  filter(any_safety_concerns>0)


HH$feel_unsafe_female.health_center
HH$feel_unsafe_male.health_center
HH$feel_unsafe_female.way_to_facilities



HH$unsafe_reason_male.road_accident
HH$unsafe_reason_female.road_accident
