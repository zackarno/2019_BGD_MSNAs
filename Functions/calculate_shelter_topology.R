
# Shelter type
ShelterType <- function(x){
  if(x>3.5){
    "Pucca"
  } else if(x>2 & x<=3.5){
    "SemiPucca"
  } else if(x>=1.75 & x<=2){
    "Kutcha"
  } else if(x<1.75){
    "Jhuprie"
  } else{
    "error"
  }
}


calculate_shelter_typology <- function(HH_data) {
  HH_data<-merge(x=HH_data, y=data.frame(X_uuid=HH_data$X_uuid,
                                         Roof=(rowSums(t(t(data.frame(HH_data$roof.tin, 
                                                                      HH_data$roof.tarpaulin,
                                                                      HH_data$roof.hay,
                                                                      HH_data$roof.brick_cemen,
                                                                      HH_data$roof.bamboo,
                                                                      HH_data$roof.wood,
                                                                      HH_data$roof.other)) * c(3,1,2,4,2,2,1))) / 
                                                 rowSums(t(t(data.frame(HH_data$roof.tin, 
                                                                        HH_data$roof.tarpaulin,
                                                                        HH_data$roof.hay,
                                                                        HH_data$roof.brick_cemen,
                                                                        HH_data$roof.bamboo,
                                                                        HH_data$roof.wood,
                                                                        HH_data$roof.other)) * c(3,1,2,4,2,2,1))>0)),
                                         Wall=(rowSums(t(t(data.frame(HH_data$wall.bricks, 
                                                                      HH_data$wall.clay,
                                                                      HH_data$wall.bamboo,
                                                                      HH_data$wall.cement,
                                                                      HH_data$wall.tarpaulin,
                                                                      HH_data$wall.wood,
                                                                      HH_data$wall.tin,
                                                                      HH_data$wall.other)) * c(4,2,2,4,1,2,2,1))) / 
                                                 rowSums(t(t(data.frame(HH_data$wall.bricks, 
                                                                        HH_data$wall.clay,
                                                                        HH_data$wall.bamboo,
                                                                        HH_data$wall.cement,
                                                                        HH_data$wall.tarpaulin,
                                                                        HH_data$wall.wood,
                                                                        HH_data$wall.tin,
                                                                        HH_data$wall.other)) * c(4,2,2,4,1,2,2,1))>0)),
                                         Floor=(rowSums(t(t(data.frame(HH_data$floor.bricks, 
                                                                       HH_data$floor.cement,
                                                                       HH_data$floor.wood,
                                                                       HH_data$floor.dirt,
                                                                       HH_data$floor.other)) * c(3,4,2,1,1))) / 
                                                  rowSums(t(t(data.frame(HH_data$floor.bricks, 
                                                                         HH_data$floor.cement,
                                                                         HH_data$floor.wood,
                                                                         HH_data$floor.dirt,
                                                                         HH_data$floor.other)) * c(3,4,2,1,1))>0))),
                 by.x=c("X_uuid"), by.y=c("X_uuid"), all.x=TRUE)
  HH_data$ShelterScore<-rowMeans(data.frame(HH_data$Roof,HH_data$Wall,HH_data$Floor))
  HH_data$ShelterType<-sapply(HH_data$ShelterScore,ShelterType)
  
  
}



# HH_data$ShelterScore<-rowMeans(data.frame(HH_data$Roof,HH_data$Wall,HH_data$Floor))
# HH_data$ShelterType<-sapply(HH_data$ShelterScore,ShelterType)




