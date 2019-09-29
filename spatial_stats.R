rm(list=ls())
user<-"zack"
population<-c("Host","Refugee")[2]
data_process<-c("checking", "cleaning", "analysis")[3]
analysis_phase<-c("basic","relationship_testing")[1]

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
library(tmap)
library(gapminder)
library(plotly)
library(ggplot2)
tmap_mode("view")

source("Functions/colours.R")
source("Functions/ActivatePaths.R")
source("Functions/make_composite_indicators_bgd_msna_2019_mk.R")
# source("Functions/make_composite_indicators_bgd_msna_2019.R")
source("Functions/calculate_shelter_topology.R")
source("Functions/general_utils.R")
source("Functions/recode_to_severity_bgd_msna2019.R")

source("Functions/recoding_severity/recode_SNFI_severity_bgd2019.R")
source("Functions/recoding_severity/recode_HEALTH_severity_bgd2019.R")
source("Functions/recoding_severity/recode_COPING_severity_bgd2019.R")
source("Functions/recoding_severity/recode_EDUCATION_severity_bgd2019.R")
source("Functions/recoding_severity/recode_WASH_severity_bgd2019.R")
source( "Functions/recoding_severity/recode_PROTECTION_severity_bgd2019.R")
source( "Functions/recoding_severity/recode_FOODSECURITY_severity_bgd2019.R")
source("Functions/recoding_severity/calculate_INTERSECTORAL_severity_bgd2019.R")
#LOAD DATA
#############
HH_kobo_questions<-read.csv(survey_path,stringsAsFactors = FALSE)
HH_kobo_choices<-read.csv(choices_path, stringsAsFactors = FALSE)
wash_combo_table<-read.csv(wash_severity_combination_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA, "NA"))
wash_errors<- read.csv(wash_error_path,na.strings = c("", " "))

incidents_per_camp<-read.csv("Inputs/DAPs/Severity Analysis_Protection_Reported Incidents per camp_REACH.csv", stringsAsFactors = FALSE, na.strings=c("", " ", NA, "NA"))


HH<-read.csv(HH_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA, "NA"))
Indiv<-read.csv(Indiv_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA, "NA"))
HH_kobo_questionnaire<-koboquest::load_questionnaire(HH,questions = HH_kobo_questions,choices = HH_kobo_choices, choices.label.column.to.use = "label..english")
pop<- read.csv(pop_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA))
################
#FILTER TO ONLY YES CONSENT AND MAKE COMPOSTIES
################
HH_yes_consent<- HH %>% filter(informed_consent=="yes")
indiv_yes_consent<-Indiv %>% filter(X_submission__uuid %in% HH_yes_consent$X_uuid)
# debugonce(make_composite_indicators_bgd_msna_2019)


enough_water_cols_to_fix<-HH_yes_consent %>% select(starts_with("enough_water")) %>% colnames()
rows_to_fix<-which(HH_yes_consent$X_uuid %in% wash_errors$uuid)
HH_yes_consent[rows_to_fix,enough_water_cols_to_fix] <-sapply(HH_yes_consent[rows_to_fix,enough_water_cols_to_fix],function(x)x<-NA) 


composite_indicators<-make_composite_indicators_bgd_msna_2019(hh_data = HH_yes_consent,  individual_data = indiv_yes_consent,population = population)

HH_with_composite<-HH_yes_consent %>% left_join(composite_indicators$household_composites,by="X_uuid")

HH_with_composite<-butteR::remove_concat_select_multiple(HH_with_composite,questionnaire = HH_kobo_questionnaire)

# debugonce(recode_HEALTH_severity_bgd2019)

HH_severity<-HH_with_composite

HH_severity<- recode_COPING_severity_bgd2019(HH_severity, individual_data = indiv_yes_consent, population=population)
HH_severity<- recode_WASH_severity_bgd2019(HH_severity, individual_data = indiv_yes_consent, population=population, wash_combo_table = wash_combo_table)
HH_severity<-recode_SNFI_severity_bgd2019(hh_data= HH_severity, individual_data=indiv_yes_consent, population=population)
HH_severity<-recode_HEALTH_severity_bgd2019(hh_data= HH_severity, individual_data=indiv_yes_consent, population=population)
HH_severity<-recode_EDUCATION_severity_bgd2019(hh_data= HH_severity, individual_data=indiv_yes_consent, population=population)
HH_severity<-recode_PROTECTION_severity_bgd2019(hh_data= HH_severity, individual_data=indiv_yes_consent, population=population)
HH_severity<-recode_FOODSECURITY_severity_bgd2019(hh_data= HH_severity, individual_data=indiv_yes_consent, population=population)

all_sectoral_severity_scores<-HH_severity %>% select(intersect(starts_with("sev_score"), ends_with(".total"))) %>%colnames()

HH_severity<-calculate_INTERSECTORAL_severity_bgd_msna2019(HH_severity,sectoral_scores =all_sectoral_severity_scores,coping_strategy_score = "sev_score.coping.total" )




data_process<-c("checking", "cleaning", "analysis")[3]
source("Functions/ActivatePaths.R")

hh_dirty<-read.csv(HH_path, na.string=c("NA", " ", "",NA))
sf::st_make_grid()

hh<-HH_severity %>% left_join(hh_dirty %>% select(X_uuid,X_gps_reading_longitude, X_gps_reading_latitude), by= "X_uuid")

strata_boundary<-sf::st_read(dsn = strata_boundary_gdb, strata_boundary_layer)
st_crs(strata_boundary)


?sf::st_crs
hh_sf<-st_as_sf(hh,coords = c("X_gps_reading_longitude", "X_gps_reading_latitude")) %>%
                  st_set_crs(st_crs(strata_boundary))
hh_sf<-st_transform(hh_sf,32646)
strata_boundary<-st_transform(strata_boundary,32646)
sf::


st_crs(hh_sf)
plot(hh_sf)

hh_sf$intersectoral_severity.total
my_map<-tm_shape(hh_sf) +
  tm_dots(col = "intersectoral_severity.total")

grid_size<-grid_sizes[i]
shelter_count_colname<- paste0("shelters_per_grid_",as.character(grid_size), "_m")
pop_dens_colname<- paste0("pop_dens_sqkm_per_grid_",as.character(grid_size), "_m")
grid_name<-paste0(grid_size, "_m")
grid <- st_make_grid(strata_boundary,
                     100,
                     # Kms
                     crs = 32646,
                     what = "polygons",
                     square = FALSE
)

coarse_dat <- hh_sf %>% 
  # get the bounding box
  st_bbox() %>% 
  # turn into an sfc object
  st_as_sfc() %>% 
  # negative buffer 
  st_buffer(-4) %>% 
  # make a square grid
  st_make_grid(cellsize = 100) %>% 
  # turn into sf object
  st_sf()

landscapetools::show_landscape(hh_sf) + 
  geom_sf(data = coarse_dat, alpha = 0.5)


library(grainchanger)

coarse_dat$shdi_3 <- winmove_agg(coarse_dat = coarse_dat, 
                                 fine_dat = hh_sf,
                                 d = 3,
                                 type = "rectangle", 
                                 win_fun = hh_sf, 
                                 agg_fun = mean,
                                 is_grid = FALSE,
                                 lc_class = 1:4)
HH_severity$c
coarse_dat <- st_read(system.file("shape/poly_sf.shp", package="grainchanger"))

coarse_dat$var_range <- nomove_agg(coarse_dat = coarse_dat,
                                   fine_dat = cont_ls,
                                   agg_fun = var_range,
                                   is_grid = FALSE)

ggplot(coarse_dat, aes(fill = var_range)) + 
  geom_sf() + 
  theme_bw()
