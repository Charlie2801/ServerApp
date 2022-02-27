#scripts for loading data into the app
#load environmental variables
load('myEnvironment.RData')
#full information on each simulation run for each municipality
full_overview <<- read.csv2("data/Complete_Simulations_Info_Update_Final.csv", colClasses = c('AGS'='character'))

#data about mean infections in municipalities with AGS
map_overview_data <<- read.csv('data/map_infection_overview.csv')

#links to all simulation data
link_data <<- read.csv2("data/munc_links.csv") %>% 
  filter(! is.na(Link)) %>% 
  filter(! Link =="")

#links to all municipality info data (syntezised populations)
link_population_data <<-  read.csv2("data/population_links.csv") %>%
  filter(! is.na(Link)) %>% 
  filter(! Link =="")

#Sizes and coordinations of all households of every municipality
household_info_list <<- read.csv('data/household_sizes.csv', colClasses = c('AGS'='character'))


#------------------load shapes for maps--------------------------------------------------
codes = c (
  "051540004004", "051540020020", "051540028028", "051540040040", "051540064064", "051620028028", "051700004004", "051700036036", "053340020020", "053580036036", "053580052052", "053660024024", "053660036036", "053700008008", "053700024024", "053740028028", "053820052052", "055540032032",
  "055540040040", "055540064064", "055580008008", "055580020020", "055580036036", "055580040040", "055660004004", "055660056056", "055660072072", "055660092092", "055700032032", "055700040040", "055700048048", "057540052052", "057620032032", "057660028028", "057660052052", "057700016016",
  "057700036036", "057740012012", "057740028028", "057740040040", "059580008008", "059580048048", "059620008008", "059620048048", "059620056056", "059660008008", "059660016016", "059740004004", "059740008008", "059740012012", "059740024024", "059740032032", "059740036036", "059740048048",
  "059740056056"
)

munc <<- readOGR(dsn = "data/shapes/VG250_GEM.shp", stringsAsFactors = F)  %>% 
  subset (ARS %in% codes)

counties <<- readOGR(dsn = "data/shapes/VG250_GEM.shp", stringsAsFactors = F)%>% 
  subset (SN_L == "05")

#Prepare Data 
shp_df <<- broom::tidy(munc) %>% 
  left_join(rio ::import("data/munc_codes.csv", colClasses = rep("character", 3)), 
            by = c("id" = "Geo_Code"))
cnames <- aggregate(cbind(long, lat) ~ id, data=shp_df, FUN=mean) %>%
  left_join(rio ::import("data/munc_codes.csv", colClasses = rep("character", 3)), 
            by = c("id" = "Geo_Code"))

#Create basic Map of NRW
map <- ggplot() + 
  geom_polygon(data = counties,
               aes(x = long, y = lat, group = group), 
               colour = "black", fill = "#ECECEC")+ 
  geom_polygon(data = shp_df,
               aes(x = long, y = lat, group = group), 
               colour = "black", fill = "blue") + 
  geom_text(data = cnames, aes(x = long, y = lat, label = Name), size = 2, color = 'white') + 
  theme_void()




#create map for munc
nrw <<- readOGR(dsn = "data/shapes/dvg2rbz_nw.shp", stringsAsFactors = F)  



#adding regbez to full_overview:
addRegBez = function(){
  
  full_overview_new = full_overview
  
  full_overview_new$RegBez = NA
  for(i in 1:nrow(full_overview_new)){
    for(j in 1:length(nrw$KN)){
      if(substr(as.character(nrw$KN[j]), 2, 3) == substr(full_overview_new$AGS[i], 1, 2)){
        full_overview_new$RegBez[i] = nrw$GN[j]
      }
    }
  }
  
  
  
  return(full_overview_new)
}

full_overview <<- addRegBez()




