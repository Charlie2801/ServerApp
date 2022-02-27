library(tidyverse)
library(foreign)


save.image(file='myEnvironment.RData')

NrPlaceTypes = function(){
  data = read.dbf("C:/Users/charl/OneDrive/Desktop/NRW Grouped unzipped/Muenster RegBez/gis_osm_pois_free_1.dbf", as.is = T)
  data$code = as.character(data$code)
  
  #filter no of leisure places
  leisure_name = 'fast_food'
  work_name = 'bank'
  
  frame = data.frame(Place_Type = c(), Occurance = c())
  
  names = data$fclass
  nrLeisure = sum(names == leisure_name)
  
  nrWork = sum(names == work_name)
  
  nrSchool = sum(names == 'school')
  
  nrUniversity = sum(names == 'university')
  
  nrKindergarten = sum(names == 'kindergarten')
  
  nrSupermarket = sum(names == 'supermarket')
  
  frame = rbind(frame, data.frame(Place_Type = 'Leisure', Occurance = nrLeisure))  
  frame = rbind(frame, data.frame(Place_Type = 'Work', Occurance = nrWork))
  frame = rbind(frame, data.frame(Place_Type = 'School', Occurance = nrSchool))
  frame = rbind(frame, data.frame(Place_Type = 'Kindergarten', Occurance = nrKindergarten))
  frame = rbind(frame, data.frame(Place_Type = 'University', Occurance = nrUniversity)) 
  frame = rbind(frame, data.frame(Place_Type = 'supermarket', Occurance = nrSupermarket))  
  
  
  return(frame)
}


AddRegBez = function(){
  file = read.csv2('C:/Users/charl/OneDrive/Desktop/ShinyDashboardCovidSimulation/data/Complete_Simulations_Info.csv')
  
  munc = unique(file$name)
  
  reg = c('Duesseldorf', 'Muenster', 'Arnsberg', 'Detmold', 'Arnsberg', 'Arnsberg', 'Duesseldorf', 'Arnsberg', 'Muenster', 'Detmold', 'Arnsberg', 'Arnsberg', 'Detmold', 'Koeln', 'Muenster', 'Detmold',  'Muenster', 'Duesseldorf', 'Koeln', 'Duesseldorf', 'Arnsberg', 'Duesseldorf', 'Detmold', 'Detmold', 'Koeln', 'Arnsberg', 'Muenster', 'Arnsberg', 'Koeln', 'Koeln', 'Arnsberg', 'Koeln', 'Muenster', 'Muenster', 'Detmold', 'Muenster', 'Muenster', 'Duesseldorf', 'Muenster', 'Arnsberg', 'Koeln', 'Arnsberg', 'Duesseldorf', 'Koeln', 'Koeln', 'Muenster', 'Detmold', 'Muenster', 'Muenster', 'Duesseldorf', 'Arnsberg', 'Detmold', 'Muenster', 'Arnsberg', 'Arnsberg')
  
  
  
  frame = data.frame(Munc = munc, RegBez = reg)
  
  file[, 'OSGov'] <- NA
  
  file[, 'OSLei'] <- NA
  file[, 'OSWork'] <- NA
  file[, 'OSSchool'] <- NA
  file[, 'OSKinder'] <- NA
  file[, 'OSUni'] <- NA
  file[, 'OSSuper'] <- NA
  
  occurances = data.frame(Gov = c('Arnsberg', 'Detmold', 'Duesseldorf', 'Koeln', 'Muenster'), 
                          nrLeisure = c(6104, 3890, 9278, 9111, 4038), nrWork = c(1535, 1160, 1954, 1989, 1112), nrSchool = c(197, 189, 310, 288, 194), nrKindergarten = c(275, 198, 601, 457, 270), nrUniversity = c(52, 21, 41, 58, 127), nrSupermarket = c(653, 491, 1008, 765, 534))
  
  for(i in 1:nrow(file)){
    for(j in 1:nrow(frame)){
      if(file$name[i] == frame$Munc[j]){
        file$OSGov[i] = frame$RegBez[j]
      } 
    }
    
    for(j in 1:nrow(occurances)){
      if(file$OSGov[i] == occurances$Gov[j]){
        file$OSWork[i] = occurances$nrWork[j]
        file$OSSchool[i] = occurances$nrSchool[j]
        file$OSKinder[i] = occurances$nrKindergarten[j]
        file$OSLei[i] = occurances$nrLeisure[j]
        file$OSUni[i] = occurances$nrUniversity[j]
        file$OSSuper[i] = occurances$nrSupermarket[j]
      }
    }
  }
  

  
  
  
  write.csv2( file, 'C:/Users/charl/OneDrive/Desktop/ShinyDashboardCovidSimulation/data/Complete_Simulations_Info_Update.csv')
  
    
}

AddInfPlaces = function(){
  frame = data.frame(name = NA, batchId = NA, NrLeisure = NA, NrWork = NA, NrSchool = NA, NrKindergarten = NA, NrUni = NA, NrSupermarket = NA)
  
  for(i in 1:nrow(link_data)){
    print(link_data$Name[i])
    allRuns = loadMunc(link_data$Link[i])$full_data
  #allRuns = loadMunc("https://uni-muenster.sciebo.de/s/yMJWfXrfNUpabAz")$full_data
  
    for(j in 1:30){
      currBatch = allRuns[allRuns$Batch_ID == j,]
      XNrLeisure = currBatch %>%
        select(place_type, location.lat,location.lon) %>% 
        filter(place_type == 'leisure') %>% 
        unique()
      XNrWork = currBatch %>%
        select(place_type, location.lat,location.lon) %>% 
        filter(place_type == 'workplace') %>% 
        unique()
      XNrSchool = currBatch %>%
        select(place_type, location.lat,location.lon) %>% 
        filter(place_type == 'school') %>% 
        unique()
      XNrKindergarten = currBatch %>%
        select(place_type, location.lat,location.lon) %>% 
        filter(place_type == 'kindergarten') %>% 
        unique()
      XNrUni = currBatch %>%
        select(place_type, location.lat,location.lon) %>% 
        filter(place_type == 'university') %>% 
        unique()
      XNrSupermarket = currBatch %>%
        select(place_type, location.lat,location.lon) %>% 
        filter(place_type == 'supermarket') %>% 
        unique()
      
      frame = rbind(frame, data.frame(name = link_data$Name[i], batchId = j, NrLeisure = nrow(XNrLeisure), NrWork = nrow(XNrWork), NrSchool = nrow(XNrSchool), NrKindergarten = nrow(XNrKindergarten), NrUni = nrow(XNrUni), NrSupermarket = nrow(XNrSupermarket)) ) 
      # = rbind(frame, data.frame(name = "Alpen", batchId = j, NrLeisure = nrow(XNrLeisure), NrWork = nrow(XNrWork), NrSchool = nrow(XNrSchool), NrKindergarten = nrow(XNrKindergarten), NrUni = nrow(XNrUni), NrSupermarket = nrow(XNrSupermarket)) ) 
      
    }
    
    
  }
  
  print('Working on CSV')
  
  file = read.csv2('C:/Users/charl/OneDrive/Desktop/ShinyDashboardCovidSimulation/data/Complete_Simulations_Info_Update.csv')
  
  file[, 'MuncLei'] <- NA
  file[, 'MuncWork'] <- NA
  file[, 'MuncSchool'] <- NA
  file[, 'MuncKindergarten'] <- NA
  file[, 'MuncUniversity'] <- NA
  file[, 'MuncSupermarket'] <- NA
  
  for(i in 1:nrow(file)){
    
    file$MuncLei[i] = as.numeric(unlist(frame %>%
      filter(batchId == file$batchId[i] & name == file$name[i]) %>%
      select(NrLeisure)))
    
    
    file$MuncWork[i] = as.numeric(unlist(frame %>%
      filter(batchId == file$batchId[i] & name == file$name[i]) %>%
      select(NrWork)))
    
    file$MuncSchool[i] = as.numeric(unlist(frame %>%
      filter(batchId == file$batchId[i] & name == file$name[i]) %>%
      select(NrSchool)))
    
    file$MuncKindergarten[i] = as.numeric(unlist(frame %>%
      filter(batchId == file$batchId[i] & name == file$name[i]) %>%
      select(NrKindergarten)))
    
    file$MuncUniversity[i] = as.numeric(unlist(frame %>%
      filter(batchId == file$batchId[i] & name == file$name[i]) %>%
      select(NrUni)))
    
    file$MuncSupermarket[i] = as.numeric(unlist(frame %>%
      filter(batchId == file$batchId[i] & name == file$name[i]) %>%
      select(NrSupermarket)))
    
  }
  
  
  write.csv2(file, 'C:/Users/charl/OneDrive/Desktop/ShinyDashboardCovidSimulation/data/Complete_Simulations_Info_Update_Final.csv')
  

}                  




##loop for the creation of the stored images
for(i in 1:length(link_data$Name)){
  fig = timeCases(i)
  selected_munc = link_data$Name[i]
  assign(paste("Z_", selected_munc, sep = ""), fig)
}


##loop for the creation of the stored map images
for(i in 1:length(link_data$Name)){
  print(link_data$Name[i])
  fig = muncMap(link_data$Name[i])
  selected_munc = link_data$Name[i]
  assign(paste("Z_Maps_", selected_munc, sep = ""), fig)
}

completeDensityMapNRW <<- createCompleteDensityMap()

