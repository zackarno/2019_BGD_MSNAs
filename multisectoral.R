
# devtools::install_github("ellieallien/Setviz")


#RUN SEVERITY RMD UP TO LINE 233 TO LOAD REQUIRED OBJECTS

HH_srv$variables$weights<-weights(HH_srv)

HH_data<-data.frame(HH_srv$variables)


greater_3_cols<-HH_srv$variables %>% select(ends_with("greater_3")) %>% colnames()
greater_3_cols<-greater_3_cols[greater_3_cols %in% c("int.sev_pts.protection.mental_phys.total.greater_3","int.sev_score.protection.mental_phys.total.greater_3","intersectoral_severity.total.greater_3")==FALSE]


newnames<-c("COPING", "WASH", "SNFI", "HEALTH", "EDUCATION", "PROTECTION", "FOOD_SECURITY")
setnames(HH_data, old = greater_3_cols, new = newnames)


if(population=="Refugee"){
  multisectoral_label= "Refugee - Intersecting Multisectoral Needs"
}
if(population=="Host"){
  multisectoral_label= "Host Community - Intersecting Multisectoral Needs"
}
population

multisectoral_above_3<-Setviz::plot_set_percentages(data=HH_data,varnames = newnames,weight_variable = "weights",exclude_unique = FALSE,label =multisectoral_label,nintersects = 12)
?Setviz::plot_set_percentages
multisectoral_above_3

greater_3_cols<-greater_3_cols[greater_3_cols %in% c("sev_score.coping.total.greater_3")==FALSE]
HH_srv$variables[,greater_3_cols]<-lapply(HH_srv$variables[,greater_3_cols],function(x)(as.numeric(as.character(x))))
HH_srv$variables$number_sectors_over3<-rowSums(HH_srv$variables[,greater_3_cols], na.rm = TRUE) %>% as.factor()

HH_srv %>% 
  group_by(number_sectors_over3) %>% 
  summarise(number_sectors_over3_mean= survey_mean(na.rm=TRUE, vartype="ci")) %>% write.csv("2019_09_26_Host_multisectoral_number_sectors_over3.csv")


HH_srv %>% 
  group_by(number_sectors_over3) %>% 
  summarise(number_sectors_over3_mean= survey_mean(na.rm=TRUE, vartype="ci")) %>% 
  ggplot(aes(x=number_sectors_over3,y=number_sectors_over3_mean))+ geom_point(colour=rgb(238/255,88/255,89/255))+
  scale_y_continuous(breaks=seq(0,1, by=0.1),labels = scales::percent_format(accuracy = 1))+
  geom_line(group=1, colour=rgb(238/255,88/255,89/255),size=1.5 )+theme_clean() +labs(x= "Number of Sectors 4 and Above", y= "Percent HHs")+
  ggtitle(paste0(population, " - # Sectors 4 and above"))
  ggsave(paste0(population," Multisectoral_Number_Sectors_Over3.png"), device = "png", type = "cairo")

getwd()
HH_data[,greater_3_cols]<-lapply(HH_data[,greater_3_cols],function(x)(as.numeric(as.character(x))))
<-rowSums(HH_data[,greater_3_cols])






# food consumption --------------------------------------------------------
food_types_with_FCS<-c("cereals_tubers", 
              "pulses_nuts_seeds",
              "vegetables",
              "fruits",
              "dairy",
              "meat_fish",
              "oil_fats",
              "sweets",
              "spices_condiments", "I.FCS_score")
food_types_with_FCS_rescaled<-c("cereals_tubers", 
                       "pulses_nuts_seeds",
                       "vegetables",
                       "fruits",
                       "dairy",
                       "meat_fish",
                       "oil_fats",
                       "sweets",
                       "spices_condiments", "fcs_rescale_1_7")

food_types<-c("cereals_tubers", 
                       "pulses_nuts_seeds",
                       "vegetables",
                       "fruits",
                       "dairy",
                       "meat_fish",
                       "oil_fats",
                       "sweets",
                       "spices_condiments")
pca_with_fcs<-princomp( sapply(HH_srv$variables[,food_types_with_FCS],scale))
pca_just_food_rescaled<-princomp(HH_srv$variables[,food_types_with_FCS_rescaled])
pca_just_food_rescaled2<-princomp(sapply(HH_srv$variables[,food_types_with_FCS_rescaled],scale))
pca_just_food<-princomp(HH_srv$variables[,food_types])
biplot(pca_with_fcs, col=c("grey","black"), cex=c(1/8, 1))
biplot(pca_just_food, col=c("grey","black"), cex=c(1/8, 1))
biplot(pca_just_food_rescaled, col=c("grey","black"), cex=c(1/8, 1))
biplot(pca_just_food_rescaled2, col=c("grey","black"), cex=c(1/8, 1))

HH_srv$variables$fcs_rescale_1_7<-scales::rescale(HH_srv$variables$I.FCS_score, to = c(1, 7))
M<-cor(HH_srv$variables[,food_types])
M<-corrplot::      HH_srv$variables[,food_types]
asdf<-corrplot::corrplot(M, method="number")

ggplot(HH_srv$variables,aes(x=fruits, y= dairy))+geom_jitter()
plot(as.numeric(HH_srv$variables$fruits),as.numeric(HH_srv$variables$dairy ))

scale(HH_srv$variables$I.FCS_score,1:7)
asdf<-princomp( sapply(HH_srv$variables[,food_types],scale))
HH_srv$variables$I.FCS_score
biplot(asdf, col=c("grey","black"), cex=c(1/8, 1))


HH_srv$variables$vegetables



lapply(HH_data[,greater_3_cols], )
rowSums(HH_data[,greater_3_cols])


# multisectoral_above_3<-Setviz::plot_set_percentages(data=HH_data,varnames = greater_3_cols,weight_variable = "weights")
multisectoral_above_3


HH_svy_ob$variables$hoh_gender
HH_svy_ob$variables$cooking_fuel.purchased_firewood

aggregated.results <- svymean(HH_svy_ob$variables[, c("cooking_fuel.purchased_firewood","hoh_gender")], 
                              HH_svy_ob, na.rm = T)



HH_srv$variables<-HH_srv$variables %>% 
  mutate(
    ed_fs= sev_score.education.total.greater_3&sev_score.FS.total.greater_3,
    fs_snfi=sev_score.FS.total.greater_3 &sev_score.snfi.total.greater_3
  )


HH_srv$variables[,c("ed_fs", "fs_snfi")]<-lapply(HH_srv$variables[,c("ed_fs", "fs_snfi")], as.factor)

asdf<-butteR::mean_proportion_table(design = HH_srv,list_of_variables = c("ed_fs", "fs_snfi"),aggregation_level = NULL,round_to = 2,return_confidence = FALSE,na_replace = FALSE,questionnaire = HH_kobo_questionnaire)



# msni radar plot test ----------------------------------------------------
weighting_function <- hypegrammaR::map_to_weighting(sampling.frame = pop,
                                                    data = HH_severity,
                                                    sampling.frame.population.column =sf_strata,
                                                    sampling.frame.stratum.column = sf_pop,
                                                    data.stratum.column = strata)

HH_srv$variables %>% select(intersect(starts_with("sev_score"), ends_with(".total"))) %>% colnames() %>% dput()
sectoral_totals<-c("sev_score.wash.total", "sev_score.snfi.total", 
  "sev_score.health.total", "sev_score.education.total", "sev_score.protection.total", 
  "sev_score.FS.total")

HH_srv$variables[,sectoral_totals]<-lapply(HH_srv$variables[,sectoral_totals],
                                           function(x) as.numeric(as.character(x)))

HH_srv$variables$group=1
# debugonce(msni19::radar_graph)
radar_plot<-msni19::radar_graph(HH_srv$variables, 
                    lsg = c("sev_score.education.total", 
                            "sev_score.snfi.total", 
                            "sev_score.FS.total", 
                            "sev_score.health.total", 
                            "sev_score.protection.total",
                            "sev_score.wash.total"),
                    lsg_labels = c("Education in\nEmergency",
                                   "Emergency\nShelter\nand NFI",
                                   "Food Security and\nAgriculture",
                                   "Health",
                                   "Protection",
                                   "WASH"),
                    group = "Upazila",
                    # group_labels = c("Everyone"),
                    weighting_function = weighting,
                    print_plot = F,
                    plot_name = "radar",
                    path = "graphs")


radar_plot  
HH_srv$variables$camp_name %>% class()
HH_srv$variables$Upazila %>% class()
HH_srv$variables$Upazila
??msni19::radar_plot
?radar_plot
HH_srv$variables$camp_name
radar_plot
