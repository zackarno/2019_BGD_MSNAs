population<-c("Host","Refugee")[2]
data_process<-c("checking", "cleaning", "analysis")[1]
source("Functions/ActivatePaths.R")
HH<-read.csv(HH_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA, "NA"))
Indiv<-read.csv(Indiv_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA, "NA"))
footprint_gdb<-"../../../01_GIS_BASE_Data/02_landscape/01_infrastructure/01_shelter_footprint/03_unosat_footprints"
st_layers(footprint_gdb)
strata_boundary_gdb %>% st_layers()

camp_boundary_dissolved<-st_read(strata_boundary_gdb, "190310_Outline_Rohingya_Refugee_Camp_A1_dissolved")

ggplot(camp_boundary_dissolved)+geom_sf()


st_crs(camp_boundary_dissolved)
HH_sf<-st_as_sf ( HH, coords= c("X_gps_reading_longitude","X_gps_reading_latitude"), crs=st_crs(camp_boundary_dissolved))
if(population=="Host"){
distance_pts<-st_distance(HH_sf, camp_boundary_dissolved)
HH_sf<-HH_sf %>% 
  mutate(euc_dist_to_camp_m= as.numeric(distance_pts) )
HH_df<-HH_sf %>% data.frame()
}
if population("Refugee"){
  camp_lines<-st_cast(camp_boundary_dissolved, "MULTILINESTRING")
  distance_pts<-st_distance(HH_sf, camp_lines)
  HH_sf<-HH_sf %>% 
    mutate(euc_dist_to_camp_m= as.numeric(distance_pts))
}


# ggplot(HH_sf)+geom_histogram(aes(x=euc_dist_to_camp_m, fill=factor(upazilla_name)))
# ggplot(HH_sf)+geom_histogram(aes(x=euc_dist_to_camp_m))

if (population=="Refugee"){
grid_extent<- st_transform(camp_boundary_dissolved,32646)
footprint<- st_read(footprint_gdb, "BGD_Camp_ShelterFootprint_UNOSAT_REACH_v1_07may2019")
}
footprint.utm<- st_transform(footprint, 32646)
shelters<-footprint.utm %>% filter(area_m2>=3.25)
shelter_centroids<- sf::st_centroid(shelters)

grid_sizes<- c(50,100, 200)
sq_km_conversion<-c(400, 100,50)
start_time<-Sys.time()
grid_with_shelter_counts<-list()
for( i in 1: length(grid_sizes)){
  print(Sys.time())
  print(i)
  grid_size<-grid_sizes[i]
  shelter_count_colname<- paste0("shelters_per_grid_",as.character(grid_size), "_m")
  pop_dens_colname<- paste0("pop_dens_sqkm_per_grid_",as.character(grid_size), "_m")
  grid_name<-paste0(grid_size, "_m")
  grid <- st_make_grid(grid_extent,
                       grid_size,
                       # Kms
                       crs = 32646,
                       what = "polygons",
                       square = TRUE
  )
  
  grid <- st_sf(index = 1:length(lengths(grid)), grid) # Add index
  
  
  shelters_on_grid<-st_join(shelter_centroids,grid, left=F)

  
  shelters_per_grid<-shelters_on_grid %>% 
    group_by(index) %>%
    summarise(!!shelter_count_colname:=n()) %>% 
    mutate( !!pop_dens_colname:= !!sym(shelter_count_colname)*sq_km_conversion[i]* 5 )
  print("last_step")
  grid_with_shelter_counts[[grid_name]]<- grid %>% 
    left_join(st_drop_geometry(shelters_per_grid), by= "index" ) 


  }

end_time<-Sys.time()
end_time-start_time

HH_sf.utm<-st_transform(HH_sf, 32646)

for (i in 1: length(grid_with_shelter_counts)){
  HH_sf.utm<- st_join(HH_sf.utm ,grid_with_shelter_counts[[i]], left=T)
  }

dem_30m_path<-"../../../01_GIS_BASE_Data/02_landscape/02_natural_features/01_elevation/01_Satellite/BGD_SRTM_30m_46N.tif"

library(raster)
dem_30m.utm<-raster(dem_30m_path)
?extract
HH_sf.utm$dem_30m<-raster::extract(dem_30m.utm,HH_sf.utm)


population<-c("Host","Refugee")[2]
data_process<-c("checking", "cleaning", "analysis")[3]
HH<-read.csv(HH_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA, "NA"))


env_columns<-HH_sf.utm %>% as.data.frame() %>% dplyr::select(dem_30m, starts_with("pop_dens"),
                     starts_with("shelters_per_grid")) %>% colnames()

clean_data_with_environmentl<-HH %>% 
  left_join(HH_sf.utm %>%as.data.frame() %>% 
              dplyr::select(X_uuid,euc_dist_to_camp_m,env_columns
                                                                                        ))
if (population=="Refugee"){
write.csv(clean_data_with_environmentl,"Inputs/Refugee/04_data_analysis/cleaned_data/20190915_HH_Refugee_Cleaned_20190917_with_environmental.csv")}
  


# write.csv(clean_data_with_distance,"Inputs/Host_Community/03_data_analysis/cleaned_datasets/20190909_HH_HostCommunity_Cleaned_20190915_with_dist.csv")


