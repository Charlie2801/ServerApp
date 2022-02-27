


#function for coloring the map by certain variables.
#Requires Data frame with AGS and Data as columns
ColorMap = function(Color_By_Data){
  
  Color_By_Data = Color_By_Data %>% 
    dplyr::select(AGS,Data)
  
  if(is.numeric(Color_By_Data$AGS)){
    Color_By_Data$AGS = paste("0",as.character(Color_By_Data$AGS), sep ="")
  }
  
  shp_df_new = shp_df %>% 
    left_join(Color_By_Data, 
              by = c("AGS" = "AGS"))
  
  map + geom_polygon(data = shp_df_new,
                     aes(x = long, y = lat, group = group, fill = Data), 
                     colour = "black") + 
        #scale_fill_viridis_b()+
        scale_fill_gradient2(low = "blue", high = "red")+
        #geom_text(data = cnames, aes(x = long, y = lat, label = Name), size = 2, color = 'white') + 
        theme_void()
  
}


createMap = function(muni){
  #search for link for municipality information in link_population_data
  link = (link_data %>% filter(Name == muni))$Link
  
  #get municipality data from Sciebo
  muni = (loadMunc(link))$median_dataset
  
  proj4string(nrw)
  proj4string(munc)
  new_munc = spTransform(munc, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  nrw = spTransform(nrw, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  new_shp_df = broom::tidy(new_munc) %>% 
    left_join(rio ::import("data/munc_codes.csv", colClasses = rep("character", 3)), 
              by = c("id" = "Geo_Code"))
  
  ggplot() +
    geom_polygon(data = nrw, aes( x = long, y = lat, group = group), fill="#ECECEC", color="black") +
    geom_polygon(data = (new_shp_df %>% filter(Name == muni$Batch_Name[1])),
                 aes(x = long, y = lat, group = group), 
                 color = "black",  fill = "#ECECEC") + 
    geom_point(data=muni, aes(location.lon, location.lat), inherit.aes = FALSE, alpha = 0.5, size = 0.5) + coord_equal() +
    theme_void() 
  
  
  
}

createDensityMap = function(muni){
  #search for link for municipality information in link_population_data
  link = (link_data %>% filter(Name == muni))$Link
  
  #get municipality data from Sciebo
  muni = (loadMunc(link))$median_dataset
  
  proj4string(nrw)
  proj4string(munc)
  new_munc = spTransform(munc, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  nrw = spTransform(nrw, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  new_shp_df = broom::tidy(new_munc) %>% 
    left_join(rio ::import("data/munc_codes.csv", colClasses = rep("character", 3)), 
              by = c("id" = "Geo_Code"))
  
  ggplot(data=muni, aes(location.lon, location.lat)) +
    geom_polygon(data = nrw, aes( x = long, y = lat, group = group), fill="#ECECEC", color="black") +
    geom_polygon(data = (new_shp_df %>% filter(Name == muni$Batch_Name[1])),
                 aes(x = long, y = lat, group = group), 
                 color = "black",  fill = "#ECECEC") + 
    stat_density2d(data=muni, aes(fill = ..level..), geom = "polygon", alpha = 0.2, ) + coord_equal() + scale_fill_viridis_c() +
    theme_void() 
  
  
  
}

createCompleteDensityMap = function(){
  #for base-plot
  proj4string(nrw)
  nrw = spTransform(nrw, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  proj4string(munc)
  new_munc = spTransform(munc, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  new_shp_df = broom::tidy(new_munc) %>% 
    left_join(rio ::import("data/munc_codes.csv", colClasses = rep("character", 3)), 
              by = c("id" = "Geo_Code"))
  
  
  
  #going through each municipality
  print(link_data$Name[1])
  
  #search for link for municipality information in link_population_data
  link = link_data$Link[1]
  
  #get municipality data from Sciebo
  muni = (loadMunc(link))$median_dataset
  
  for(i in 2 :nrow(link_data)){
    print(link_data$Name[i])
    
    #search for link for municipality information in link_population_data
    link = link_data$Link[i]
    
    #get municipality data from Sciebo
    muni = rbind(muni, (loadMunc(link))$median_dataset)
    
    
    
  }
  
  
  
  ggplot(data=muni, aes(location.lon, location.lat)) +
    geom_polygon(data = nrw, aes( x = long, y = lat, group = group), fill="#ECECEC", color="black") +
    geom_polygon(data = new_shp_df, #(new_shp_df %>% filter(Name %in%  unique(muni$Batch_Name))),
                 aes(x = long, y = lat, group = group), 
                 color = "black",  fill = "#ECECEC") + 
    stat_density2d(data=muni, aes(fill = ..level..), geom = "polygon", alpha = 0.2, ) + coord_equal() + scale_fill_viridis_c() +
    theme_void() 
  
  
  
  
  
  
}