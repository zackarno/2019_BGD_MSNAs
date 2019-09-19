
if(user=="zack"){
  strata_boundary_gdb<-"Inputs/GIS"
  
  if(population=="Host"){
    if(data_process=="cleaning"){
      data_cleaning_input_folder<-"Inputs/Host_Community/04_data_cleaning/"
      data_cleaning_input_folder_contents<-dir("Inputs/Host_Community/04_data_cleaning/")
      HH_file<-data_cleaning_input_folder_contents[data_cleaning_input_folder_contents%>% stringr::str_ends("HH_Data.csv")]
      HH_path<-paste0(data_cleaning_input_folder,HH_file)
      Indiv_file<-data_cleaning_input_folder_contents[data_cleaning_input_folder_contents%>% stringr::str_ends("INDIV_Data.csv")]
      Indiv_path<-paste0(data_cleaning_input_folder,Indiv_file)}
    
    if (data_process=="analysis"){
      HH_path<- "Inputs/Host_Community/03_data_analysis/cleaned_datasets/20190909_HH_HostCommunity_Cleaned_20190915_with_dist.csv"
      Indiv_path<-"Inputs/Host_Community/03_data_analysis/cleaned_datasets/20190909_Indiv_HostCommunity_Cleaned_20190915.csv"
      strata<- "union_name" 
      sf_pop<-'HH_pop'
      strata_boundary_layer<-"BGD_Teknaf_Ukhia_Unions"
      dap_population_name<-"hc_only" 
      sf_strata<- "Union"
    }
    if( data_process=="checking"){
      HH_path<-"Inputs/Host_Community/02_data_collection/HH_HC_MSNA.csv"
      Indiv_path<-"Inputs/Host_Community/02_data_collection/INDIV_HC_MSNA.csv"
      strata<- "adm4_en"
    samples_required_data_path<-"Inputs/Host_Community/02_data_collection/sample_requests_pop_numbers/kmz_sample_request_template - samples_by_union_msna2019.csv"
    sample_data_path<- "Inputs/Host_Community/sample_requests_pop_numbers/Population_Figures_2011_Census_HostCommunity.csv"
    audit_dir<-"Inputs/Host_Community/01_pilot/01_audit/67a9bdb3f03e4583aad322026e7dd9e0/"
    date_log_path<-"Inputs/Host_Community/02_data_collection/02_data_logged/date_logger.csv"
    audit_node<-"/azB6PDS7fxdp26j5dDJXSp/"
    
    ##########CHANGE WHEN SWITCHING FROM PILOT TO DC ###############3
    HH_sensitive_info_removed_path<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\01_HostCommunity/02_data_collection/daily_data/",str_replace_all(ymd(Sys.Date()-1),"-","_"),"_HH_Data.csv")
    Indiv_sensitive_info_removed_path<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\01_HostCommunity/02_data_collection/daily_data/",str_replace_all(ymd(Sys.Date()-1),"-","_"),"_INDIV_Data.csv")
    # target_points_gdb<- "GIS/Sampling/Host_Community/Pilot_R1/pilot_r1_comprehensive.kml"
    # target_points<-readOGR(target_points_gdb, "HC_MSNA_R1_Comprehensive")
    target_points_gdb<- "GIS/Sampling/Host_Community/R1/HC_MSNA_R1_Comprehensive.kml"
    target_points<-readOGR(target_points_gdb,"HC_MSNA_R1_Comprehensive")
    

    
    
    ###################
    path_unzip <- "Inputs/Host_Community/02_data_collection/98_temp"
    audit_zip_dir<-"Inputs/Host_Community/02_data_collection/01_audit"
    # audit_zipfile <- "Inputs/Host_Community/01_pilot/01_audit/axiD4wAprsz6g3zGVjveVh_2019_08_05_13_01_08 (1).zip"
    ########CHANGE WHEN SWITCH FROM PILOT TO DC#########
    copy_zip_to<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\01_HostCommunity\\02_data_collection/audit/",Sys.Date()-1,".zip")
    
    sf_strata<- "Union"
    
    
    aux_other_response_indiv<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\01_HostCommunity/02_data_collection/aux_outputs/otherResponses/",
                                     str_replace_all(ymd(Sys.Date()-2),"-","_"),"_INDIV_OtherResponses.csv")
    aux_other_response_hh<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\01_HostCommunity/02_data_collection/aux_outputs/otherResponses/",
                                  str_replace_all(ymd(Sys.Date()-2),"-","_"),"_HH_OtherResponses.csv")
    aux_NA_response_indiv<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\01_HostCommunity/02_data_collection/aux_outputs/NAresponses/",
                                  str_replace_all(ymd(Sys.Date()-2),"-","_"),"_INDIV_NAResponses.csv")
    aux_NA_response_hh<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection/01_HostCommunity/02_data_collection/aux_outputs/NAresponses/",
                               str_replace_all(ymd(Sys.Date()-2),"-","_"),"_HH_NAresponses.csv")
    aux_duration_path<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\01_HostCommunity/02_data_collection/aux_outputs/duration_data/", str_replace_all(ymd(Sys.Date()-2),"-","_"),"_Response_durations_cum.csv")
    buffer<-50    }
    survey_path<-"Inputs/Host_Community/02_data_collection/04_tool/HostCommunity_MSNA2019_tool_survey.csv"
    choices_path<- "Inputs/Host_Community/02_data_collection/04_tool/HostCommunity_MSNA2019_tool_choices.csv"
    pop_path<-"Inputs/Host_Community/03_data_analysis/Population_Figures_2011_Census_HostCommunity.csv"

    
  }
  
  #REFUGEE FILE PATHS
  #####################################################################
  if(population=="Refugee"){
    if(data_process=="cleaning"){
      data_cleaning_input_folder<-"Inputs/Refugee/data_cleaning/"
      data_cleaning_input_folder_contents<-dir("Inputs/Refugee/data_cleaning/")
      HH_file<-data_cleaning_input_folder_contents[data_cleaning_input_folder_contents%>% stringr::str_ends("HH_Data.csv")]
      HH_path<-paste0(data_cleaning_input_folder,HH_file)
      Indiv_file<-data_cleaning_input_folder_contents[data_cleaning_input_folder_contents%>% stringr::str_ends("INDIV_Data.csv")]
      Indiv_path<-paste0(data_cleaning_input_folder,Indiv_file)
    }
    if (data_process=="analysis"){
      HH_path<- "Inputs/Refugee/04_data_analysis/cleaned_data/20190915_HH_Refugee_Cleaned_20190917.csv"
      Indiv_path<-"Inputs/Refugee/04_data_analysis/cleaned_data/20190915_Indiv_Refugee_Cleaned_20190917.csv"
      strata<- "camp_name" 
      sf_pop<-'Total.Families'
      sf_strata<-"camp_id"
      strata_boundary_layer<-"190310_Outline_Rohingya_Refugee_Camp_A1"
      dap_population_name<-"ref_only" 
      
    }
    if (data_process== "checking") {
      HH_path<- "Inputs/Refugee/02_data_collection/HH_Refugee.csv"
      Indiv_path<-"Inputs/Refugee/02_data_collection/INDIV_Refugee_MSNA.csv"
    # sample_data_path<-"Inputs/Refugee/.."
    # audit_dir<-"Inputs/Refugee/01_raw_data/01_audit/"
    date_log_path<-"Inputs/Refugee/02_data_collection/02_data_logged/date_logger.csv"
    HH_sensitive_info_removed_path<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\02_Refugee\\02_data_collection/daily_data/", str_replace_all(ymd(Sys.Date()-2),"-","_"),"_HH_Data.csv")
    Indiv_sensitive_info_removed_path<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\02_Refugee\\02_data_collection/daily_data/", str_replace_all(ymd(Sys.Date()-2),"-","_"),"_INDIV_Data.csv")
    samples_required_data_path<-"Inputs/Refugee/03_sampling/03_sample_requests_pop_numbers/kmz_sample_request_template - sample_by_camp_msna2019.csv"
    
    
    
    path_unzip <- "Inputs/Refugee/02_data_collection/98_temp"
    audit_zip_dir<-"Inputs/Refugee/02_data_collection/01_audit"
    # audit_zipfile <- "Inputs/Refugee/02_data_collection/01_audit/aU6HGytRQLdhhu6m9bdwvA_2019_08_05_11_04_32.zip"
    copy_zip_to<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\02_Refugee/02_data_collection/audit/",Sys.Date()-2,".zip")
    # strata="New_Camp_N"
    # sf_strata= "Camp_Name"
    target_points_gdb<- "GIS/Sampling/Refugee/DC_R1/Refugee_DC_R1_SamplePoints_Comprehensive.kml"
    target_points<-readOGR(target_points_gdb,"Refugee_DC_R1_SamplePoints_Comprehensive")
    audit_node<-"/aU6HGytRQLdhhu6m9bdwvA/"
    aux_other_response_indiv<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\02_Refugee\\02_data_collection\\aux_outputs/otherResponses/",
                                     str_replace_all(ymd(Sys.Date()-2),"-","_"),"_INDIV_OtherResponses.csv")
    aux_other_response_hh<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\02_Refugee\\02_data_collection\\aux_outputs/otherResponses/",
                                  str_replace_all(ymd(Sys.Date()-2),"-","_"),"_HH_OtherResponses.csv")
    aux_NA_response_indiv<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\02_Refugee\\02_data_collection\\aux_outputs/NAresponses/",
                                  str_replace_all(ymd(Sys.Date()-2),"-","_"),"_INDIV_NAResponses.csv")
    aux_NA_response_hh<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\02_Refugee\\02_data_collection\\aux_outputs/NAresponses/",
                               str_replace_all(ymd(Sys.Date()-2),"-","_"),"_HH_NAresponses.csv")
    aux_duration_path<-paste0("D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\02_Refugee\\02_data_collection\\aux_outputs/duration_data/",
                              str_replace_all(ymd(Sys.Date()-2),"-","_"),"_Response_durations_cum.csv")
    buffer<-25
    referral_path<-"D:/Dropbox/REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\02_Refugee\\02_data_collection\\aux_outputs/referrals/"
    }
    pop_path<-"Inputs/Refugee/04_data_analysis/01_population_figures/july2019_unhcr_pop_numbers.csv"
    survey_path<-"Inputs/Refugee/02_data_collection/04_tool/Refugee_MSNA2019_tool_survey.csv"
    choices_path<-"Inputs/Refugee/02_data_collection/04_tool/Refugee_MSNA2019_tool_choices.csv"}
 
  long_lat<-c("X_gps_reading_longitude","X_gps_reading_latitude")}