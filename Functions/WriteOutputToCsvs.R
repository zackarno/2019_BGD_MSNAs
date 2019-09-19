write.csv(HH_Desensitized,  HH_sensitive_info_removed_path)
write.csv(Indiv,Indiv_sensitive_info_removed_path)

if(population=="Refugee"){
  write.csv(referrals_desensitized, paste0(referral_path,str_replace_all(ymd(Sys.Date()),"-","_"),"_RefugeeReferrals.csv"))}



write.csv(date_log_full,date_log_path,row.names=FALSE)

write.csv( short_times_on_questions_table,aux_duration_path)

write.csv( HH_other,aux_other_response_hh )
write.csv(Indiv_other, aux_other_response_indiv)
write.csv(Na_HH_table, aux_NA_response_hh)
write.csv(Na_Indiv_table, aux_NA_response_indiv)